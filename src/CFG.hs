module CFG where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad (when)
import Data.Maybe(fromMaybe, isNothing)
import qualified Data.Map as M
import qualified AbsLatte as L
import IR
import System.IO (hPrint, stderr)
import System.Exit (exitWith, ExitCode (ExitFailure), exitFailure)

data Outlinks
  = None
  | Single Blabel
  | Cond Llabel Blabel Blabel
  deriving(Show, Eq)

data CFGBlock
  = CFGBlock {
    label :: Blabel,
    outLinks :: Outlinks,
    inLinks :: [Blabel],
    quads :: [IExpr],
    vars :: M.Map L.Ident Llabel
  }
  deriving(Show, Eq)

data Store
  = Store {
    idx :: Int,
    bidx :: Int,
    curLabel :: Blabel,
    blocks :: M.Map Blabel CFGBlock
  }
  deriving(Show, Eq)

data BFSOutput
  = BFSOutput {
    visited :: [Blabel],
    to_visit :: [Blabel]
  }

type CFGIM a = StateT Store IO a
type OutputIM a = StateT BFSOutput IO a

getFromMapOrErr :: Ord a => a -> M.Map a b -> CFGIM b
getFromMapOrErr key map = do
  let v = M.lookup key map in
    case v of
      Nothing -> liftIO $ print "Error that should never happen unless compilator is faulty\n"
        >> exitWith (ExitFailure 1)
      Just x -> return x

getFromMapOrErr2 :: Ord a => a -> M.Map a b -> OutputIM b
getFromMapOrErr2 key map = do
  let v = M.lookup key map in
    case v of
      Nothing -> liftIO $ print "Error that should never happen unless compilator is faulty\n"
        >> exitWith (ExitFailure 1)
      Just x -> return x

showLlabelIR :: Int -> Llabel
showLlabelIR v = Llabel $ "i_" ++ show v

showBlabelIR :: Int -> Blabel
showBlabelIR v = Blabel $ "b_" ++ show v

genLlabelIR :: CFGIM Llabel
genLlabelIR = do
  s <- get
  put $ Store (idx s + 1) (bidx s) (curLabel s) (blocks s)
  return $ showLlabelIR $ idx s + 1

genBlabelIR :: CFGIM Blabel
genBlabelIR = do
  s <- get
  put $ Store (idx s) (bidx s + 1) (curLabel s) (blocks s)
  return $ showBlabelIR $ bidx s + 1

genMostRecentLabelIR :: CFGIM Llabel
genMostRecentLabelIR = do
  gets (showLlabelIR . idx)

eval2arg :: L.Expr -> L.Expr -> CFGIM ([IExpr], Llabel, [IExpr], Llabel)
eval2arg a b = do
  ira <- convertAbsToIR a
  la <- genMostRecentLabelIR
  irb <- convertAbsToIR b
  lb <- genMostRecentLabelIR
  return (ira, la, irb, lb)

convertAbsToIR :: L.Expr -> CFGIM [IExpr]
convertAbsToIR (L.EVar _ x) = do
  l <- genLlabelIR
  curB <- getCurrentBlock
  lx <- getFromMapOrErr x $ vars curB
  return [IExpr l $ Simple $ ILabel lx]
convertAbsToIR (L.ELitInt _ c) = do
  l <- genLlabelIR
  return [IExpr l $ Simple $ ILitInt c]
convertAbsToIR (L.ELitTrue _) = do
  l <- genLlabelIR
  return [IExpr l $ Simple ILitTrue]
convertAbsToIR (L.ELitFalse _) = do
  l <- genLlabelIR
  return [IExpr l $ Simple ILitFalse]
convertAbsToIR (L.EApp _ ident args) = do
  l <- genLlabelIR
  argsConv <- mapM convertAbsToIR args
  argsRegisters <- mapM getRegisterFromList argsConv
  return $ IExpr l (Simple $ IApp ident argsRegisters) : concat argsConv
  where
  getRegisterFromList :: [IExpr] -> CFGIM Llabel
  getRegisterFromList [IExpr lhs _] = return lhs
  getRegisterFromList (x:xt) = getRegisterFromList xt
convertAbsToIR (L.EString _ s) = do
  l <- genLlabelIR
  return [IExpr l $ Simple $ ILitString s]
convertAbsToIR (L.Neg _ x) = do
  irx <- convertAbsToIR x
  mostRecent <- genMostRecentLabelIR
  l <- genLlabelIR
  return $ IExpr l (INeg $ ILabel mostRecent) : irx
convertAbsToIR (L.Not _ x) = do
  irx <- convertAbsToIR x
  mostRecent <- genMostRecentLabelIR
  l <- genLlabelIR
  return $ IExpr l (INot $ ILabel mostRecent) : irx
convertAbsToIR (L.EMul _ a op b) = do
  (ira, la, irb, lb) <- eval2arg a b
  l <- genLlabelIR
  return $ IExpr l (IMul op la lb) : irb ++ ira -- FIXME mniejszy do większego
convertAbsToIR (L.EAdd _ a op b) = do
  (ira, la, irb, lb) <- eval2arg a b
  l <- genLlabelIR
  return $ IExpr l (IAdd op la lb) : irb ++ ira
convertAbsToIR (L.ERel _ a op b) = do
  (ira, la, irb, lb) <- eval2arg a b
  l <- genLlabelIR
  return $ IExpr l (IRel op la lb) : irb ++ ira
convertAbsToIR (L.EAnd _ a b) = do
  (ira, la, irb, lb) <- eval2arg a b
  l <- genLlabelIR
  return $ IExpr l (IAnd la lb) : irb ++ ira
convertAbsToIR (L.EOr _ a b) = do
  (ira, la, irb, lb) <- eval2arg a b
  l <- genLlabelIR
  return $ IExpr l (IOr la lb) : irb ++ ira

changeBlock :: Blabel -> CFGBlock -> CFGIM ()
changeBlock bl block = do
  s <- get
  put $ s {blocks = M.insert bl block $ blocks s}

getBlock :: Blabel -> CFGIM CFGBlock
getBlock b = do
  s <- get
  getFromMapOrErr b $ blocks s

blockChangeOutlink :: Blabel -> Outlinks -> CFGIM ()
blockChangeOutlink from link = do
  bfrom <- getBlock from
  changeBlock from $ bfrom {outLinks = link}

blockAddInlink :: Blabel -> Blabel -> CFGIM ()
blockAddInlink from to = do
  bto <- getBlock to
  changeBlock to $ bto {inLinks = from : inLinks bto}

blockAddVar :: L.Ident -> Llabel -> CFGIM ()
blockAddVar x l = do
  s <- get
  let b = curLabel s
  bb <- getBlock b
  changeBlock b $ bb {vars = M.insert x l $ vars bb}

blockAddQuad :: IExpr' -> CFGIM ()
blockAddQuad q = do
  s <- get
  let b = curLabel s
  bb <- getBlock b
  l <- genLlabelIR
  changeBlock b $ bb {quads = IExpr l q : quads bb}

blockAddManyQuads :: [IExpr] -> CFGIM ()
blockAddManyQuads qs = do
  s <- get
  let b = curLabel s
  bb <- getBlock b
  changeBlock b $ bb {quads = qs ++ quads bb}

newBlockFromId :: Blabel -> CFGIM CFGBlock
newBlockFromId old = do
  bold <- getBlock old
  newBlabel <- genBlabelIR
  let newBlock = CFGBlock newBlabel (outLinks bold) [] [] (vars bold) in do
    changeBlock newBlabel newBlock
    return newBlock

getCurrentBlock :: CFGIM CFGBlock
getCurrentBlock = do
  s <- get
  getBlock $ curLabel s

getCurrentBlabel :: CFGIM Blabel
getCurrentBlabel = gets curLabel

blockAddOutgoingInlinks :: CFGIM ()
blockAddOutgoingInlinks = do
  curB <- getCurrentBlock
  case outLinks curB of
    None -> return ()
    Single nxt -> do
      blockAddQuad $ Simple $ IBr0 nxt
      blockAddInlink (label curB) nxt
    Cond c a b -> do
      blockAddQuad $ Simple $ IBrCond c a b
      blockAddInlink (label curB) a
      blockAddInlink (label curB) b

blockMakePhi :: [CFGBlock] -> L.Ident -> CFGIM IExpr
blockMakePhi inblocks v = do
  curB <- getCurrentBlock
  l <- genLlabelIR
  blockVs <- mapM (getFromBlock v) inblocks
  changeBlock (label curB) $ curB {vars = M.insert v l $ vars curB}
  return $ IExpr l $ Simple $ IPhi v blockVs
  where
  getFromBlock :: L.Ident -> CFGBlock -> CFGIM (Blabel, Llabel)
  getFromBlock v b = do
    bv <- getFromMapOrErr v $ vars b
    return (label b, bv)

blockAddInitialPhis :: CFGIM ()
blockAddInitialPhis = do
  curB <- getCurrentBlock
  inblocks <- mapM getBlock $ inLinks curB
  let inheritedVars = map fst $ M.toList $ vars curB
  varPhis <- mapM (blockMakePhi inblocks) inheritedVars
  curB <- getCurrentBlock
  unless (null (quads curB)) (liftIO $ print "INVALIDATING SOME QUADS!!!")
  changeBlock (label curB) $ curB {quads = quads curB ++ varPhis}

repairPhis :: CFGIM ()
repairPhis = do
  curB <- getCurrentBlock
  inblocks <- mapM getBlock $ inLinks curB
  let inheritedVars = map fst $ M.toList $ vars curB
  let all_quads = quads curB
  phisList <- mapM (blockMakePhi inblocks) inheritedVars
  let phisMap = M.fromList $ map phiExtract phisList
  repaired_quads <- mapM (repair phisMap) all_quads
  changeBlock (label curB) $ curB {quads = repaired_quads}
  where
  phiExtract :: IExpr -> (L.Ident, [(Blabel, Llabel)])
  phiExtract (IExpr _ (Simple (IPhi x lst))) = (x, lst)
  repair :: M.Map L.Ident [(Blabel, Llabel)] -> IExpr -> CFGIM IExpr
  repair phisMap (IExpr l (Simple (IPhi x _))) = do
    real_lst <- getFromMapOrErr x phisMap
    return $ IExpr l $ Simple $ IPhi x real_lst
  repair phisMap x = return x

changeBlockAndConvertFunc :: Blabel -> [L.Stmt' L.BNFC'Position] -> CFGIM()
changeBlockAndConvertFunc newBlabel body = do
  s <- get
  put $ s {curLabel = newBlabel}
  blockAddInitialPhis
  relay body
  where
  relay [L.BStmt _ (L.Block _ stmts)] = convertFuncToIR stmts
  relay st = convertFuncToIR st

convertFuncToIR :: [L.Stmt' L.BNFC'Position] -> CFGIM ()
convertFuncToIR [] = blockAddOutgoingInlinks
convertFuncToIR ((L.Empty _):st) = convertFuncToIR st
convertFuncToIR ((L.SExp _ e):st) = convertFuncToIR st
convertFuncToIR ((L.BStmt _ (L.Block _ stmts)):st) = do
  curBlabel <- getCurrentBlabel
  xtb <- getCurrentBlock
  afterBlock <- newBlockFromId curBlabel
  blockChangeOutlink curBlabel $ Single $ label afterBlock
  inBlock <- newBlockFromId curBlabel
  blockChangeOutlink curBlabel $ Single $ label inBlock
  blockAddOutgoingInlinks
  changeBlockAndConvertFunc (label inBlock) stmts
  changeBlockAndConvertFunc (label afterBlock) st
convertFuncToIR ((L.Decl _ type_ idents):st) = do
  mappings <- addDecls type_ idents
  curB <- getCurrentBlock
  changeBlock (label curB) $ curB {vars =  M.union (M.fromList mappings) $ vars curB}
  convertFuncToIR st
  where
  addDecls :: L.Type -> [L.Item] -> CFGIM [(L.Ident, Llabel)]
  addDecls _ [] = return []
  addDecls (L.Int _p) ((L.NoInit _ x):dt) = do
    blockAddQuad $ Simple $ ILitInt 0
    l <- genMostRecentLabelIR
    rest <- addDecls (L.Int _p) dt
    return $ (x, l) : rest
  addDecls (L.Bool _p) ((L.NoInit _ x):dt) = do
    blockAddQuad $ Simple ILitFalse
    l <- genMostRecentLabelIR
    rest <- addDecls (L.Bool _p) dt
    return $ (x, l) : rest
  addDecls (L.Str _p) ((L.NoInit _ x):dt) = do
    blockAddQuad $ Simple $ ILitString ""
    l <- genMostRecentLabelIR
    rest <- addDecls (L.Str _p) dt
    return $ (x, l) : rest
  addDecls _t ((L.Init _ x e):dt) = do
    qs <- convertAbsToIR e
    blockAddManyQuads qs
    l <- genMostRecentLabelIR
    rest <- addDecls _t dt
    return $ (x, l) : rest
convertFuncToIR ((L.Ass _ ident e):st) = do
  es <- convertAbsToIR e
  blockAddManyQuads es
  l <- genMostRecentLabelIR
  curB <- getCurrentBlock
  changeBlock (label curB) $ curB {vars = M.insert ident l $ vars curB}
  convertFuncToIR st
convertFuncToIR ((L.Incr _d x):st) = do
  curB <- getCurrentBlock
  lx <- getFromMapOrErr x $ vars curB
  blockAddQuad $ Simple $ ILitInt 1
  l1 <- genMostRecentLabelIR
  blockAddQuad $ IAdd (L.Plus _d) lx l1
  convertFuncToIR st
convertFuncToIR ((L.Decr _d x):st) = do
  curB <- getCurrentBlock
  lx <- getFromMapOrErr x $ vars curB
  blockAddQuad $ Simple $ ILitInt 1
  l1 <- genMostRecentLabelIR
  blockAddQuad $ IAdd (L.Minus _d) lx l1
  convertFuncToIR st
convertFuncToIR ((L.Ret _ e):st) = do
  s <- get
  blockChangeOutlink (curLabel s) None
  es <- convertAbsToIR e
  blockAddManyQuads es
  l <- genMostRecentLabelIR
  blockAddQuad $ IRet $ ILabel l
convertFuncToIR ((L.VRet _):st) = do
  s <- get
  blockChangeOutlink (curLabel s) None
  blockAddQuad IVRet
convertFuncToIR ((L.Cond _ expr ifb):st) = do
  es <- convertAbsToIR expr
  blockAddManyQuads es
  let iflabel = lhs $ head es
  brLabel <- genLlabelIR
  curBlabel <- getCurrentBlabel
  afterBlock <- newBlockFromId curBlabel
  blockChangeOutlink curBlabel $ Single $ label afterBlock
  inBlock <- newBlockFromId curBlabel
  blockChangeOutlink curBlabel $ Cond iflabel (label inBlock) (label afterBlock)
  blockAddOutgoingInlinks
  changeBlockAndConvertFunc (label inBlock) [ifb]
  changeBlockAndConvertFunc (label afterBlock) st
convertFuncToIR ((L.CondElse _ expr ifok ifelse):st) = do
  es <- convertAbsToIR expr
  blockAddManyQuads es
  let iflabel = lhs $ head es
  curBlabel <- getCurrentBlabel
  brLabel <- genLlabelIR
  afterBlock <- newBlockFromId curBlabel
  blockChangeOutlink curBlabel $ Single $ label afterBlock
  okBlock <- newBlockFromId curBlabel
  elseBlock <- newBlockFromId curBlabel
  blockChangeOutlink curBlabel $ Cond iflabel (label okBlock) (label elseBlock)
  blockAddOutgoingInlinks
  changeBlockAndConvertFunc (label okBlock) [ifok]
  changeBlockAndConvertFunc (label elseBlock) [ifelse]
  changeBlockAndConvertFunc (label afterBlock) st
convertFuncToIR ((L.While _ expr wb):st) = do
  curBlabel <- getCurrentBlabel
  afterBlock <- newBlockFromId curBlabel
  whileBodyBlock <- newBlockFromId curBlabel
  whileCondBlock <- newBlockFromId curBlabel
  blockChangeOutlink curBlabel $ Single $ label whileCondBlock
  blockAddOutgoingInlinks

  s <- get
  put $ s {curLabel = label whileCondBlock}
  fakeIflabel <- genLlabelIR
  blockChangeOutlink (label whileCondBlock) $ Cond fakeIflabel (label whileBodyBlock) (label afterBlock)
  blockAddInitialPhis

  es <- convertAbsToIR expr
  let iflabel = lhs $ head es
  blockAddManyQuads es
  blockAddManyQuads [IExpr fakeIflabel $ Simple $ ILabel iflabel]
  blockAddOutgoingInlinks

  -- FIXME zmienne indukcyjne?
  blockChangeOutlink (label whileBodyBlock) $ Single $ label whileCondBlock
  changeBlockAndConvertFunc (label whileBodyBlock) [wb]
  
  s <- get
  put $ s {curLabel = label whileCondBlock} 
  repairPhis 

  changeBlockAndConvertFunc (label afterBlock) st

convertFuncsToIR :: [L.TopDef' L.BNFC'Position] -> IO ()
convertFuncsToIR [] = return ()
convertFuncsToIR ((L.FnDef pos ftype (L.Ident fname) fargs (L.Block _ fbody)):ft) = do
  let blabel = Blabel $ fname ++ "_entry"
  let bmp = M.singleton blabel $ CFGBlock blabel None [] [] M.empty
  ((), store) <- runStateT (convertFuncToIR fbody) (Store 1 1 blabel bmp)
  liftIO $ print blabel
  ((), _) <- runStateT (printBlocks (blocks store) True) (BFSOutput [] [blabel])
  convertFuncsToIR ft

printBlocks :: M.Map Blabel CFGBlock -> Bool -> OutputIM ()
printBlocks blocks suppressFirstName = do
  s <- get
  if null $ to_visit s then return () else do
  let (current : remaining) = to_visit s
  put $ s {to_visit = remaining, visited = current : visited s}
  curB <- getFromMapOrErr2 current blocks
  unless suppressFirstName $ liftIO $ print $ label curB
  displayQuads $ quads curB
  let out_blocks = outLinks curB
  case out_blocks of
    None -> return ()
    Single x -> checkNeighbours x
    Cond _ a b -> do
      checkNeighbours a
      checkNeighbours b
  printBlocks blocks False
  where
  checkNeighbours :: Blabel -> OutputIM ()
  checkNeighbours b = do
    s <- get
    if elem b (visited s) || elem b (to_visit s) then
      return ()
    else
      put $ s {to_visit = to_visit s ++ [b]}
  displayQuads :: [IExpr] -> OutputIM ()
  displayQuads [] = return ()
  displayQuads (q:qt) = do
    displayQuads qt
    liftIO $ print q

compile :: L.Program' L.BNFC'Position -> IO ()
compile (L.Program _ prog) = do
  convertFuncsToIR prog
  return ()


