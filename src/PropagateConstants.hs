module PropagateConstants where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad (when)
import Data.Maybe(fromMaybe, isNothing)
import qualified Data.Map as M
import qualified AbsLatte as L
import IR
import CFG
import qualified Data.List as LST
import System.IO (hPrint, stderr)
import System.Exit (exitWith, ExitCode (ExitFailure), exitFailure)

data PropagateStore
  = PropagateStore {
    blockMap :: M.Map Blabel CFGBlock,
    propFound :: Bool,
    toSub :: Llabel,
    subWith :: IExprSimple,
    iterCount :: Int,
    anythingFound :: Bool
  }

type PropagateConstantsIM a = StateT PropagateStore IO a

propagateConstants :: PropagateConstantsIM Bool
propagateConstants = do
  s <- get
  let blist = map snd $ M.toList $ blockMap s
  let foundConsts = map (findConst . quads) blist
  --liftIO $ print $ foundConsts
  chooseSub foundConsts
  s <- get
  --liftIO $ print $ toSub s
  if propFound s then do
    doSub
    s <- get
    put $ s {propFound = False, iterCount = iterCount s + 1, anythingFound = True}
    --when (iterCount s < 50)
    propagateConstants
  else do
    gets anythingFound

findConst :: [IExpr] -> Maybe IExpr
findConst [] = Nothing
findConst ((IExpr l (Simple (ILabel l2))):et) = Just $ IExpr l (Simple (ILabel l2))
findConst ((IExpr l (Simple (ILitInt v))):et) = Just $ IExpr l (Simple (ILitInt v))
findConst ((IExpr l (Simple ILitFalse)):et) = Just $ IExpr l (Simple ILitFalse)
findConst ((IExpr l (Simple ILitTrue)):et) = Just $ IExpr l (Simple ILitTrue)
findConst ((IExpr l (IPhi c lst)):et) = if testPhi lst l then Just $ IExpr l $ IPhi c lst else findConst et
findConst ((IExpr l (IAOrPhi lst)):et) = if testPhi lst l then Just $ IExpr l $ IAOrPhi lst else findConst et
findConst (_:et) = findConst et

testPhi :: [(Blabel, IExprSimple)] -> Llabel -> Bool
testPhi lst l = length (simplifyPhiRhs lst l) <= 1

simplifyPhiRhs :: [(Blabel, IExprSimple)] -> Llabel -> [IExprSimple]
simplifyPhiRhs lst l = LST.delete (ILabel l) $ LST.nub $ map snd lst


chooseSub :: [Maybe IExpr] -> PropagateConstantsIM ()
chooseSub [] = return ()
chooseSub (Nothing:ct) = chooseSub ct
chooseSub (Just (IExpr l (Simple simp)):ct) = do
  s <- get
  put $ s {propFound = True, toSub = l, subWith = simp}
chooseSub (Just (IExpr l (IPhi c lst)):ct) = do
  s <- get
  put $ s {propFound = True, toSub = l, subWith = head $ simplifyPhiRhs lst l}
chooseSub (Just (IExpr l (IAOrPhi lst)):ct) = do -- FIXME redundant?
  s <- get
  put $ s {propFound = True, toSub = l, subWith = head $ simplifyPhiRhs lst l}


doSub :: PropagateConstantsIM ()
doSub = do
  s <- get
  bs <- mapM substitute $ M.toList $ blockMap s
  put $ s {blockMap = M.fromList bs}
  where
  substitute :: (Blabel, CFGBlock) -> PropagateConstantsIM (Blabel, CFGBlock)
  substitute (l, b) = do
    s <- get
    let subed = toSub s
    let subBy = subWith s
    qsub <- mapM (substituteWithinQuad subed subBy) $ quads b
    return (l, b {quads = qsub})
  substituteWithinQuad :: Llabel -> IExprSimple -> IExpr -> PropagateConstantsIM IExpr
  substituteWithinQuad subed subBy (IExpr l (Simple (ILabel l2))) = do
    if l == subed then
      return KilledIExpr
    else
      if l2 == subed then
        return $ IExpr l $ Simple subBy
      else
        return $ IExpr l $ Simple (ILabel l2)
  substituteWithinQuad subed subBy (IExpr l (Simple v)) = do -- not sure about this 1
    if l == subed then
      return KilledIExpr
    else
      return $ IExpr l $ Simple v
  substituteWithinQuad subed subBy (IExpr l (IBrCond c a b)) = do
    if c /= subed then return $ IExpr l (IBrCond c a b) else
      case subBy of -- FIXME FIXME modify Outlinks!
        ILitFalse -> return $ IExpr l $ IBr0 b
        ILitTrue -> return $ IExpr l $ IBr0 a
        ILabel c2 -> return $ IExpr l $ IBrCond c2 a b
        -- FIXME add unable to optimize error
  substituteWithinQuad subed subBy (IExpr l (IPhi c phis)) = do
    if l == subed then return $ IExpr l $ Simple subBy
    else do
      phiSubed <- mapM (substituteWithinPhi subed subBy) phis
      return $ IExpr l $ IPhi c phiSubed
  substituteWithinQuad subed subBy (IExpr l (IAOrPhi phis)) = do
    phiSubed <- mapM (substituteWithinPhi subed subBy) phis
    return $ IExpr l $ IAOrPhi phiSubed
  substituteWithinQuad subed subBy (IExpr l (IApp f args)) = do
    argSubed <- mapM (substituteListOfLabels subed subBy) args
    return $ IExpr l $ IApp f argSubed
    where
    substituteListOfLabels :: Llabel -> IExprSimple -> IExprSimple -> PropagateConstantsIM IExprSimple
    substituteListOfLabels subed subBy l = do
      if l == ILabel subed then return subBy
      else return l
  substituteWithinQuad subed subBy (IExpr l (INeg x)) = do
    if x == ILabel subed then return $ IExpr l $ INeg subBy
    else return $ IExpr l $ INeg x
  substituteWithinQuad subed subBy (IExpr l (INot x)) = do
    if x == ILabel subed then return $ IExpr l $ INot subBy
    else return $ IExpr l $ INot x
  substituteWithinQuad subed subBy (IExpr l (IBinOp op a b)) = do
    (IExpr _ (Simple asub)) <- substituteWithinQuad subed subBy $ IExpr l $ Simple a
    (IExpr _ (Simple bsub)) <- substituteWithinQuad subed subBy $ IExpr l $ Simple b
    return $ IExpr l $ IBinOp op asub bsub
  substituteWithinQuad subed subBy (IExpr l (IRet x)) = do
    (IExpr _ (Simple xsub)) <- substituteWithinQuad subed subBy $ IExpr l $ Simple x
    return $ IExpr l $ IRet xsub
  substituteWithinQuad subed subBy unmatched = return unmatched -- matchowanie and/ora?

  substituteWithinPhi :: Llabel -> IExprSimple -> (Blabel, IExprSimple) -> PropagateConstantsIM (Blabel, IExprSimple)
  substituteWithinPhi subed subBy (b, l) = do
    if l == ILabel subed then return (b, subBy)
    else return (b, l)
