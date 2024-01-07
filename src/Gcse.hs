module Gcse where

import CFG
import IR
import System.IO

import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Bifunctor
import Data.List (nub)
import qualified Control.Monad

data GcseBFSStore
  = GcseBFSStore {
    gblocks :: CFGBMap,
    commonQuads :: M.Map Blabel [IExpr],
    currentQuads :: [IExpr],
    toVisit :: [Blabel],
    everSeen :: [Blabel]
  }

type GcseBFSIM a' = StateT GcseBFSStore IO a'

getCommonQuads :: Blabel -> CFGBMap -> GcseBFSIM [IExpr]
getCommonQuads b blocks = do
  gets (fromMaybe [] . M.lookup b . commonQuads)

bfsCheck :: Blabel -> GcseBFSIM ()
bfsCheck x = do
  s <- get
  xQuads <- getCommonQuads x $ gblocks s
  let curQ = nub $ currentQuads s ++ xQuads
  let xQuadsNew = if x `elem` everSeen s then filter (`elem` xQuads) curQ else curQ
  -- liftIO $ print $ show x ++ " a new block " ++ show xQuadsNew ++ " WTF " ++ show curQ ++ " WTF2 " ++ show xQuads
  if xQuads == xQuadsNew && x `elem` everSeen s then return () else
    put $ s {
      commonQuads = M.insert x xQuadsNew $ commonQuads s,
      toVisit = filter (/= x) (toVisit s) ++ [x],
      everSeen = nub $ x : everSeen s
    }

bfs :: Int -> GcseBFSIM ()
bfs ct = do
  s <- get
  if null $ toVisit s then return ()
  else do
    let (curl : toVisitRest) = toVisit s
    curQ <- getCommonQuads curl $ gblocks s
    -- liftIO $ print $ "current block: " ++ show curl ++ " and his quads" ++ show curQ ++ " and " ++ show (quads (fromJust $ M.lookup curl $ gblocks s))
    put $ s {toVisit = toVisitRest, currentQuads = quads (fromJust $ M.lookup curl $ gblocks s) ++ curQ}
    let curOutlinks = outLinks $ fromJust $ M.lookup curl $ gblocks s
    case curOutlinks of
      None -> return ()
      Single x -> bfsCheck x
      Cond _ a b -> do
        bfsCheck a
        bfsCheck b
    bfs $ ct + 1


findInOldExprs :: IExpr -> [IExpr] -> Maybe IExpr
findInOldExprs e [] = Nothing
findInOldExprs (IExpr l e) ((IExpr lo o):ot) = let eold = findInOldExprs (IExpr l e) ot in
  if isNothing eold then (if e == o && l /= lo then Just $ IExpr lo o else Nothing) else eold

findRepetitions :: [IExpr] -> [IExpr] -> IO [(Llabel, Llabel)]
findRepetitions [] oldExprs = return []
findRepetitions ((IExpr l (IApp {})):et) oldExprs = findRepetitions et oldExprs
findRepetitions ((IExpr l (IBr0 {})):et) oldExprs = findRepetitions et oldExprs
findRepetitions ((IExpr l (IBrCond {})):et) oldExprs = findRepetitions et oldExprs
findRepetitions ((IExpr l (IRet {})):et) oldExprs = findRepetitions et oldExprs
findRepetitions ((IExpr l (IVRet {})):et) oldExprs = findRepetitions et oldExprs
findRepetitions ((IExpr l e):et) oldExprs = do
  case findInOldExprs (IExpr l e) oldExprs of
    Nothing -> findRepetitions et (IExpr l e : oldExprs)
    Just (IExpr ll ee) -> do
      -- liftIO $ print $ show (IExpr l e) ++ " foundme " ++ show (IExpr ll ee)
      x <- findRepetitions et oldExprs
      if l /= ll then return $ (l, ll) : x else return x

tryReplaceLabel :: Llabel -> M.Map Llabel Llabel -> Llabel
tryReplaceLabel l substs = if M.member l substs then fromJust $ M.lookup l substs else l

replaceQuads :: M.Map Llabel Llabel -> [IExpr] -> [IExpr]
replaceQuads substs [] = []
replaceQuads substs ((IExpr l e):et) =
  if M.member l substs then replaceQuads substs et else
    IExpr l (case e of
      Simple (ILabel l) -> Simple $ ILabel $ tryReplaceLabel l substs
      IBrCond c a b -> IBrCond (tryReplaceLabel c substs) a b
      IPhi l t phiList -> IPhi (tryReplaceLabel l substs) t $ replacePhiList phiList
      IAOrPhi phiList -> IAOrPhi $ replacePhiList phiList
      IApp fname ftype fargtypes fargs -> IApp fname ftype fargtypes $ map replaceSimple fargs
      INeg e -> INeg $ replaceSimple e
      INot e -> INot $ replaceSimple e
      IBinOp op a b -> IBinOp op (replaceSimple a) (replaceSimple b)
      IAnd a b -> IAnd (replaceSimple a) (replaceSimple b)
      IOr a b -> IOr (replaceSimple a) (replaceSimple b)
      IRet r -> IRet (replaceSimple r)
      x -> x
      ) : replaceQuads substs et
  where
  replaceSimple e = case e of
    ILabel l -> ILabel $ tryReplaceLabel l substs
    e -> e
  replacePhiList = map (Data.Bifunctor.second replaceSimple)


optimizeGcse :: Blabel -> CFGBMap -> IO CFGBMap
optimizeGcse entryBlabel blocks = do
  (_, st) <- runStateT (bfs 0) $ GcseBFSStore blocks (M.singleton entryBlabel []) [] [entryBlabel] [entryBlabel]
  let commonQuadsMap = commonQuads st
  -- liftIO $ print $ show blocks
  -- mapM_ (\x -> liftIO $ print $ show (label x) ++ " " ++ show (outLinks x) ++ " " ++ show (isJust $ M.lookup (label x) commonQuadsMap) ++ "\n") blocks
  -- mapM_ (\x -> liftIO $ print $ show x ++ "\n") $ M.toList commonQuadsMap
  substs' <- mapM (\(bl, b) -> let {
    comQ = fromJust $ M.lookup bl commonQuadsMap
  } in findRepetitions (reverse $ quads b) comQ) $ M.toList blocks
  let substs = M.fromList $ concat substs'
  -- liftIO $ print $ show substs
  let blocks1 = M.map (\b -> b {quads = replaceQuads substs $ quads b}) blocks
  if blocks1 == blocks then return blocks1
    else optimizeGcse entryBlabel blocks1