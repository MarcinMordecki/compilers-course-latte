module IndVar where

import IR
import CFG
import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe

data IndVar
  = IndVar {
    phiLabel :: Llabel,
    stepLabel :: Llabel,
    step :: Int
  }

data IndStore
  = IndStore {
    indBlocks :: CFGBMap,
    lidx :: Int,
    allQuadBlockMappings :: M.Map Llabel Blabel,
    allQuads :: M.Map Llabel IExpr,
    indVars :: M.Map Llabel IndVar
}

type IndIM a = StateT IndStore IO a

makeLabelBlockMapping :: Blabel -> CFGBlock -> [(Llabel, Blabel)]
makeLabelBlockMapping blabel block = map (\(IExpr l _) -> (l, blabel)) $ quads block

stepQuad :: IExpr -> Bool
stepQuad (IExpr l (IBinOp o (ILabel _) (ILitInt _))) = o == IPlus || o == IMinus
stepQuad (IExpr l (IBinOp o (ILitInt _) (ILabel _) )) = o == IPlus -- if o == IMinus then the variable changes sign every iteration
stepQuad _ = False

checkIndQuad :: IExpr -> IndIM (Maybe IndVar)
checkIndQuad (IExpr l (IBinOp o (ILabel b) (ILitInt a))) = do
  s <- get
  let (IExpr ll e) = fromJust $ M.lookup b $ allQuads s
  case e of
    IPhi _ _ phiList -> return $ indIfReferences phiList ll
    IAOrPhi phiList -> return $ indIfReferences phiList ll
    _ -> return Nothing
  where
  indIfReferences :: [(Blabel, IExprSimple)] -> Llabel -> Maybe IndVar
  indIfReferences phiList ll =
    if phiReferencesStep phiList then
      Just $ IndVar ll l (fromInteger $ a * if o == IPlus then 1 else -1)
    else
      Nothing
  phiReferencesStep :: [(Blabel, IExprSimple)] -> Bool
  phiReferencesStep plist = ILabel l `elem` map snd plist
checkIndQuad (IExpr l (IBinOp o (ILitInt a) (ILabel b))) = checkIndQuad (IExpr l (IBinOp o (ILabel b) (ILitInt a)))


findIndVars :: IndIM ()
findIndVars = do
  s <- get
  let stepQuads = filter stepQuad $ M.elems $ allQuads s
  indVarsUnfiltered <- mapM checkIndQuad stepQuads
  put $ s {indVars = M.fromList $ concatMap (\x -> [(phiLabel x, x), (stepLabel x, x)]) $ catMaybes indVarsUnfiltered}

simplifyIndVars :: IndIM ()
simplifyIndVars = do
  findIndVars
  return ()

keepOptimizingIndVar :: IndStore -> IO (CFGBMap, Int)
keepOptimizingIndVar initialStore = do
  let initialBlocks = indBlocks initialStore
  ((), store) <- runStateT simplifyIndVars initialStore
  if initialBlocks == indBlocks store then
    return (initialBlocks, lidx store)
  else
    keepOptimizingIndVar store

optimizeIndVar :: Blabel -> Int -> CFGBMap -> IO (CFGBMap, Int)
optimizeIndVar entryBlabel lidx blocks = do
  let allQuadBlockMappings = M.fromList $ concat $ M.elems $ M.mapWithKey makeLabelBlockMapping blocks
  let allQuads = M.fromList $ map (\(IExpr l e) -> (l, IExpr l e)) $ concatMap (quads . snd) $ M.toList blocks
  (blocks1, lidx1) <- keepOptimizingIndVar $ IndStore blocks lidx allQuadBlockMappings allQuads M.empty
  return (blocks1, lidx1)