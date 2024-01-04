module PropagateConstants where

import Control.Monad.State
import qualified Data.Map as M
import qualified AbsLatte as L
import IR
import CFG
import qualified Data.List as LST
import System.IO (hPrint, stderr)
import System.Exit (exitWith, ExitCode (ExitFailure), exitFailure)

data PropagateStore
  = PropagateStore {
    blockMap :: CFGBMap,
    propFound :: Bool,
    toSub :: Llabel,
    subWith :: IExprSimple,
    iterCount :: Int,
    anythingFound :: Bool,
    invalidatedInlinks :: [(Blabel, Blabel)]
  }

type PropagateConstantsIM a = StateT PropagateStore IO a

propagateConstants :: Blabel -> PropagateConstantsIM Bool
propagateConstants entryBlabel = do
  s <- get
  let blist = map snd $ M.toList $ blockMap s
  let foundConsts = map (findConst . quads) blist
  chooseSub foundConsts
  s <- get
  if propFound s then do
    doSub entryBlabel
    s <- get
    put $ s {propFound = False, iterCount = iterCount s + 1, anythingFound = True, invalidatedInlinks = []}
    propagateConstants entryBlabel
  else do
    gets anythingFound

findConst :: [IExpr] -> Maybe IExpr
findConst [] = Nothing
findConst ((IExpr l (Simple (ILabel l2))):et) = Just $ IExpr l (Simple (ILabel l2))
findConst ((IExpr l (Simple (ILitInt v))):et) = Just $ IExpr l (Simple (ILitInt v))
findConst ((IExpr l (Simple ILitFalse)):et) = Just $ IExpr l (Simple ILitFalse)
findConst ((IExpr l (Simple ILitTrue)):et) = Just $ IExpr l (Simple ILitTrue)
findConst ((IExpr l (IPhi c t lst)):et) = if testPhi lst l then Just $ IExpr l $ IPhi c t lst else findConst et
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
chooseSub (Just (IExpr l (IPhi c t lst)):ct) = do
  s <- get
  let simp = simplifyPhiRhs lst l
  put $ s {propFound = True, toSub = l, subWith = if null simp then ILabel l else head $ simplifyPhiRhs lst l}
chooseSub (Just (IExpr l (IAOrPhi lst)):ct) = do -- FIXME redundant?
  s <- get
  put $ s {propFound = True, toSub = l, subWith = head $ simplifyPhiRhs lst l}


doSub :: Blabel -> PropagateConstantsIM ()
doSub entryBlabel = do
  s <- get
  bs <- mapM substitute $ M.toList $ blockMap s
  -- above call modifies the "invalidatedInLinks" list as well. 
  s <- get
  put $ s {blockMap = M.fromList bs}
  --mapM_ (\x -> liftIO $ print $ show (fst x) ++ " ") bs
  unreachable <- findUnreachableBlocks entryBlabel
  --liftIO $ print $ "MUY IMPORTANTE!!!!!!!!!!! " ++ show unreachable
  s <- get
  bs2 <- mapM cutInvalidatedPhi bs
  put $ s {blockMap = M.fromList $ filter (\(l, b) -> l `notElem` unreachable) bs2}
  where
  cutInvalidatedPhi :: (Blabel, CFGBlock) -> PropagateConstantsIM (Blabel, CFGBlock)
  cutInvalidatedPhi (curBlabel, b) = do
    s <- get
    let icut = map fst $ filter (\iil -> snd iil == curBlabel) $ invalidatedInlinks s
    return (curBlabel, b {
      quads = map (cutInvalidatedFromPhi icut) $ quads b,
      inLinks = filter (\x -> notElem x $ icut) $ inLinks b
      })
    where
    cutInvalidatedFromPhi :: [Blabel]  -> IExpr -> IExpr
    cutInvalidatedFromPhi icut (IExpr l (IPhi c t plist)) = IExpr l $ IPhi c t $ filter (\phi -> fst phi `notElem` icut) plist
    cutInvalidatedFromPhi icut (IExpr l (IAOrPhi plist)) = IExpr l $ IAOrPhi $ filter (\phi -> fst phi `notElem` icut) plist
    cutInvalidatedFromPhi icut e = e
  substitute :: (Blabel, CFGBlock) -> PropagateConstantsIM (Blabel, CFGBlock)
  substitute (curBlabel, b) = do
    s <- get
    let subed = toSub s
    let subBy = subWith s
    qsub <- mapM (substituteWithinQuad subed subBy) $ quads b
    newb <- checkOutlink $ b {quads = qsub}
    s <- get
    return (curBlabel, newb)
    where
    checkOutlink :: CFGBlock -> PropagateConstantsIM CFGBlock
    checkOutlink b =
      if null $ quads b then return b else do
        case outLinks b of
          Cond c l r -> do
            case head $ quads b of
              IExpr _ (IBr0 newlink) -> do
                s <- get
                put $ s {invalidatedInlinks = (label b, if newlink == l then r else l):invalidatedInlinks s}
                return $ b {outLinks = Single newlink}
              _ -> do
                return b
          _ -> return b
  substituteWithinQuad :: Llabel -> IExprSimple -> IExpr -> PropagateConstantsIM IExpr
  substituteWithinQuad subed subBy (IExpr l (Simple (ILabel l2))) = do
    if l == subed then
      return KilledIExpr
    else
      if l2 == subed then
        return $ IExpr l $ Simple subBy
      else
        return $ IExpr l $ Simple (ILabel l2)
  substituteWithinQuad subed subBy (IExpr l (Simple v)) = do -- FIXME not sure about this 1
    if l == subed then
      return KilledIExpr
    else
      return $ IExpr l $ Simple v
  substituteWithinQuad subed subBy (IExpr l (IBrCond c a b)) = do
    if c /= subed then return $ IExpr l (IBrCond c a b) else do
      case subBy of
        ILitFalse -> do
          return $ IExpr l $ IBr0 b
        ILitTrue -> do
          return $ IExpr l $ IBr0 a
        ILabel c2 -> return $ IExpr l $ IBrCond c2 a b
        _ -> fail "unable to optimize in Propagate Constants / substituteWithinQuad" -- impossible
  substituteWithinQuad subed subBy (IExpr l (IPhi c t phis)) = do
    if l == subed then return $ IExpr l $ Simple subBy
    else do
      phiSubed <- mapM (substituteWithinPhi subed subBy) phis
      return $ IExpr l $ IPhi c t phiSubed
  substituteWithinQuad subed subBy (IExpr l (IAOrPhi phis)) = do
    if l == subed then return $ IExpr l $ Simple subBy
    else do
      phiSubed <- mapM (substituteWithinPhi subed subBy) phis
      return $ IExpr l $ IAOrPhi phiSubed
  substituteWithinQuad subed subBy (IExpr l (IApp f t targs args)) = do
    argSubed <- mapM (substituteListOfLabels subed subBy) args
    return $ IExpr l $ IApp f t targs argSubed
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

findUnreachableBlocks :: Blabel -> PropagateConstantsIM [Blabel]
findUnreachableBlocks entry = do
  blocks <- gets blockMap
  --liftIO $ print "bfs"
  ((), bfsout) <- liftIO $ runStateT (bfs blocks) $ BFSOutput [] [entry]
  --liftIO $ print "bfs end"
  let unreachable = filter (\b -> label b `notElem` visited bfsout) (map snd $ M.toList blocks)
  mapM_ markOutlinks unreachable
  return $ map label unreachable
  where
  markOutlinks :: CFGBlock -> PropagateConstantsIM ()
  markOutlinks b = do
    s <- get
    case outLinks b of
      None -> return ()
      Single next -> put $ s {invalidatedInlinks = (label b, next) : invalidatedInlinks s}
      Cond _ nexta nextb -> put $ s {invalidatedInlinks = (label b, nexta) : (label b, nextb) : invalidatedInlinks s}

  bfs :: CFGBMap  -> OutputIM ()
  bfs blocks = do
    s <- get
    unless (null $ to_visit s) $ do
      let (v:rest) = to_visit s
      --liftIO $ putStr $ show v ++ "   "
      put $ s {to_visit = rest, visited = v : visited s}
      b <- getFromMapOrErr2 v blocks
      --liftIO $ putStr $ show (outLinks b) ++ " xd " ++ show (inLinks b) ++ "   " 
      case outLinks b of
        None -> return ()
        Single nextb -> bfsCheck nextb
        Cond _ nexta nextb -> do
          bfsCheck nexta
          bfsCheck nextb
      --liftIO $ putStr "\n"
      bfs blocks
  bfsCheck :: Blabel -> OutputIM ()
  bfsCheck next = do
    s <- get
    --liftIO $ putStr $ show next ++ " "
    unless (elem next (to_visit s) || elem next (visited s)) $ put $ s {to_visit = next : to_visit s}
