module CFG where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad (when)
import Data.Maybe(fromMaybe, isNothing)
import qualified Data.Map as M
import qualified AbsLatte as L
import Data.List(intersect)
import IR
import System.IO (hPrint, stderr)
import System.Exit (exitWith, ExitCode (ExitFailure), exitFailure)

data Outlinks
  = None
  | Single Blabel
  | Cond Llabel Blabel Blabel
  | CheckLast Blabel Blabel
  deriving(Show, Eq)

type CFGBMap = M.Map Blabel CFGBlock

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
    blocks :: CFGBMap
  }
  deriving(Show, Eq)

type CFGIM a = StateT Store IO a

getFromMapOrErr :: Ord a => Show b => Show a => a -> M.Map a b -> CFGIM b
getFromMapOrErr key map = do
  let v = M.lookup key map in
    case v of
      Nothing -> do
        liftIO $ print $ show key ++ " " ++ show map
        liftIO $ print "Error that should never happen unless compilator is faulty\n"
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
  return $ IExpr l (IApp ident argsRegisters) : concat argsConv
  where
  getRegisterFromList :: [IExpr] -> CFGIM IExprSimple
  getRegisterFromList [IExpr lhs _] = return $ ILabel lhs
  getRegisterFromList (x:xt) = getRegisterFromList xt
convertAbsToIR (L.EString _ s) = do
  l <- genLlabelIR
  return [IExpr l $ ILitString s]
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
  return $ IExpr l (IBinOp (genOpFromMulOp op) (ILabel la) (ILabel lb)) : irb ++ ira -- FIXME mniejszy do większego
convertAbsToIR (L.EAdd _ a op b) = do
  (ira, la, irb, lb) <- eval2arg a b
  l <- genLlabelIR
  return $ IExpr l (IBinOp (genOpFromAddOp op) (ILabel la) (ILabel lb)) : irb ++ ira
convertAbsToIR (L.ERel _ a op b) = do
  (ira, la, irb, lb) <- eval2arg a b
  l <- genLlabelIR
  return $ IExpr l (IBinOp (genOpFromRelOp op) (ILabel la) (ILabel lb)) : irb ++ ira
convertAbsToIR (L.EAnd p a b) = do
  convertAbsToIR (L.Not p (L.EOr p (L.Not p a) (L.Not p b)))
convertAbsToIR (L.EOr p a b) = do
  curBlabel <- getCurrentBlabel
  afterBlock <- newBlockFromId curBlabel
  blockChangeOutlink curBlabel $ Single $ label afterBlock
  bTrue <- newBlockFromId curBlabel
  bFalse <- newBlockFromId curBlabel

  blockChangeOutlink (label bTrue) $ Single $ label afterBlock
  blockChangeOutlink (label bFalse) $ Single $ label afterBlock

  bBlock <- newBlockFromId curBlabel
  aBlock <- newBlockFromId curBlabel

  blockChangeOutlink curBlabel $ Single $ label aBlock
  blockAddOutgoingInlinks

  blockChangeOutlink (label aBlock) $ CheckLast (label bTrue) (label bBlock)
  changeBlockAndConvertFunc (label aBlock) [L.SExp p a]

  blockChangeOutlink (label bBlock) $ CheckLast (label bTrue) (label bFalse)
  changeBlockAndConvertFunc (label bBlock) [L.SExp p b]

  focusAnotherBlock $ label bTrue
  blockAddQuad $ Simple ILitTrue
  lit_true <- genMostRecentLabelIR
  blockAddOutgoingInlinks

  focusAnotherBlock $ label bFalse
  blockAddQuad $ Simple ILitFalse
  lit_false <- genMostRecentLabelIR
  blockAddOutgoingInlinks

  focusAnotherBlock $ label afterBlock
  let cond_phi = IAOrPhi $ (label bTrue, ILabel lit_true) : [(label bFalse, ILabel lit_false)]
  blockAddQuad cond_phi
  res <- genMostRecentLabelIR
  l <- genLlabelIR
  return [IExpr l $ Simple $ ILabel res]

genOpFromMulOp :: L.MulOp' a -> ITBinOp
genOpFromMulOp (L.Times _) = IMul
genOpFromMulOp (L.Div _) = IDiv
genOpFromMulOp (L.Mod _) = IMod

genOpFromAddOp :: L.AddOp' a -> ITBinOp
genOpFromAddOp (L.Plus _) = IPlus
genOpFromAddOp (L.Minus _) = IMinus

genOpFromRelOp :: L.RelOp' a -> ITBinOp
genOpFromRelOp (L.EQU _) = IEq
genOpFromRelOp (L.NE _) = INe
genOpFromRelOp (L.LTH _) = ILt
genOpFromRelOp (L.LE _) = ILe
genOpFromRelOp (L.GTH _) = IGt
genOpFromRelOp (L.GE _) = IGe

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
      blockAddQuad $ IBr0 nxt
      blockAddInlink (label curB) nxt
    Cond c a b -> do
      blockAddQuad $ IBrCond c a b
      blockAddInlink (label curB) a
      blockAddInlink (label curB) b
    CheckLast a b -> do
      l <- genMostRecentLabelIR
      blockChangeOutlink (label curB) $ Cond l a b
      blockAddOutgoingInlinks

blockUpdateVariableTrace :: L.Ident -> CFGIM ()
blockUpdateVariableTrace ident = do
  l <- genMostRecentLabelIR
  curB <- getCurrentBlock
  changeBlock (label curB) $ curB {vars = M.insert ident l $ vars curB}

blockMakePhi :: [CFGBlock] -> L.Ident -> CFGIM IExpr
blockMakePhi inblocks v = do
  curB <- getCurrentBlock
  l <- genLlabelIR
  blockVs <- mapM (getFromBlock v) inblocks
  changeBlock (label curB) $ curB {vars = M.insert v l $ vars curB}
  return $ IExpr l $ IPhi v blockVs
  where
  getFromBlock :: L.Ident -> CFGBlock -> CFGIM (Blabel, IExprSimple)
  getFromBlock v b = do
    liftIO $ print $ show v ++ " haha " ++ show (vars b)
    bv <- getFromMapOrErr v $ vars b
    return (label b, ILabel bv)

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
  liftIO $ print "1"
  curB <- getCurrentBlock
  inblocks <- mapM getBlock $ inLinks curB
  -- FIXME verify: vars curB \cap vars any inblocks is same for any inblocks
  let inheritedVars = map fst (M.toList $ vars curB) `intersect` map fst (M.toList $ vars $ head inblocks)
  liftIO $ print inheritedVars
  let all_quads = quads curB
  liftIO $ print "2"
  phisList <- mapM (blockMakePhi inblocks) inheritedVars
  liftIO $ print "2.5"
  let phisMap = M.fromList $ map phiExtract phisList
  liftIO $ print "3"
  repaired_quads <- mapM (repair phisMap) all_quads
  liftIO $ print "4"
  if repaired_quads == quads curB then return () else do
    changeBlock (label curB) $ curB {quads = repaired_quads}
    s <- get
    case outLinks curB of
      None -> return ()
      Single x -> do
        put $ s {curLabel = x}
        repairPhis
      Cond c a b -> do
        put $ s {curLabel = a}
        repairPhis
        s <- get
        put $ s {curLabel = b}
        repairPhis
      CheckLast a b -> do -- redundant?
        put $ s {curLabel = a}
        repairPhis
        s <- get
        put $ s {curLabel = b}
        repairPhis
  where
  phiExtract :: IExpr -> (L.Ident, [(Blabel, IExprSimple)])
  phiExtract (IExpr _ (IPhi x lst)) = (x, lst)
  repair :: M.Map L.Ident [(Blabel, IExprSimple)] -> IExpr -> CFGIM IExpr
  repair phisMap (IExpr l (IPhi x _)) = do
    real_lst <- getFromMapOrErr x phisMap
    return $ IExpr l $ IPhi x real_lst
  repair phisMap x = return x

focusAnotherBlock :: Blabel -> CFGIM ()
focusAnotherBlock newBlabel = do
  s <- get
  put $ s {curLabel = newBlabel}
  blockAddInitialPhis

changeBlockAndConvertFunc :: Blabel -> [L.Stmt' L.BNFC'Position] -> CFGIM()
changeBlockAndConvertFunc newBlabel body = do
  focusAnotherBlock newBlabel
  relay body
  where
  relay [L.BStmt _ (L.Block _ stmts)] = convertFuncToIR stmts
  relay st = convertFuncToIR st

convertFuncToIR :: [L.Stmt' L.BNFC'Position] -> CFGIM ()
convertFuncToIR [] = blockAddOutgoingInlinks
convertFuncToIR ((L.Empty _):st) = convertFuncToIR st
convertFuncToIR ((L.SExp _ e):st) = do -- will be removed later - unless it's used for AND/OR
  es <- convertAbsToIR e
  blockAddManyQuads es
  convertFuncToIR st
convertFuncToIR ((L.BStmt _ (L.Block _ stmts)):st) = do
  curBlabel <- getCurrentBlabel
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
    blockAddQuad $ ILitString ""
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
  blockUpdateVariableTrace ident
  convertFuncToIR st
convertFuncToIR ((L.Incr _ x):st) = do
  curB <- getCurrentBlock
  lx <- getFromMapOrErr x $ vars curB
  blockAddQuad $ IBinOp IPlus (ILabel lx) (ILitInt 1)
  blockUpdateVariableTrace x
  convertFuncToIR st
convertFuncToIR ((L.Decr _d x):st) = do
  curB <- getCurrentBlock
  lx <- getFromMapOrErr x $ vars curB
  blockAddQuad $ IBinOp IMinus (ILabel lx) (ILitInt 1)
  blockUpdateVariableTrace x
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
  liftIO $ print "before repair"
  repairPhis
  liftIO $ print "after repair"

  changeBlockAndConvertFunc (label afterBlock) st




