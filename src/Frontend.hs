module Frontend where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad (when)
import Data.Maybe(fromMaybe, isNothing)
import qualified Data.Map as M
import qualified AbsLatte as L
import System.IO (hPrint, stderr)

newtype FrontendError = Err String
  deriving (Show)

data Objtype
  = TInt
  | TString
  | TBool
  | TFun FunType
  | TVoid
  deriving (Eq)
instance Show Objtype where
  show TInt = "int"
  show TString = "string"
  show TBool = "boolean"
  show TFun {} = "function"
  show TVoid = "void"

data TVar
    = TVar {ident :: L.Ident, varType :: Objtype}
    deriving(Show, Eq)

data FunType
  = Fun {name :: L.Ident, args :: [TVar], retType :: Objtype}
  deriving (Show, Eq)

data Env
  = Env {
    vars :: M.Map L.Ident TVar,
    currentBlockVars :: M.Map L.Ident TVar,
    currentRet :: Objtype
  }
  deriving (Show, Eq)

newtype Store
  = Store {
     functions :: M.Map L.Ident FunType
    }
  deriving(Show, Eq)

type IM a = ExceptT FrontendError (StateT Store (ReaderT Env IO)) a

addPredefinedFunctions :: IM ()
addPredefinedFunctions = do
  modify $ \s -> Store $ M.insert (L.Ident "printInt") (Fun (L.Ident "printInt") [TVar (L.Ident "dummy") TInt] TVoid) (functions s)
  modify $ \s -> Store $ M.insert (L.Ident "printString")
    (Fun (L.Ident "printString") [TVar (L.Ident "dummy") TString] TVoid) (functions s)
  modify $ \s -> Store $ M.insert (L.Ident "error") (Fun (L.Ident "error") [] TVoid) (functions s)
  modify $ \s -> Store $ M.insert (L.Ident "readInt") (Fun (L.Ident "readInt") [] TInt) (functions s)
  modify $ \s -> Store $ M.insert (L.Ident "readString") (Fun (L.Ident "readString") [] TString) (functions s)

displayPos :: L.BNFC'Position -> String
displayPos (L.BNFC'Position line col) = "at: line = " ++ show line ++ ", col = " ++ show col

displayName :: L.Ident -> String
displayName (L.Ident ident) = ident

convertType :: L.Type' L.BNFC'Position -> IM Objtype
convertType (L.Int _) = return TInt
convertType (L.Bool _) = return TBool
convertType (L.Str _) = return TString
convertType (L.Void _) = return TVoid
convertType (L.Fun pos _ _) = throwError $ Err $ "invalid fun return type " ++ displayPos pos

convertTypeVar :: L.Arg' L.BNFC'Position -> IM TVar
convertTypeVar (L.Arg pos (L.Int _) x) = return $ TVar x TInt
convertTypeVar (L.Arg pos (L.Bool _) x) = return $ TVar x TBool
convertTypeVar (L.Arg pos (L.Str _) x) = return $ TVar x TString
convertTypeVar (L.Arg pos (L.Void _) x) = throwError $ Err $ "invalid variable type (void) " ++ displayPos pos
convertTypeVar (L.Arg pos f x) = throwError $ Err $ "invalid variable type (fun) " ++ displayPos pos

convertTypeFunArgs :: [L.Arg' L.BNFC'Position] -> IM [TVar]
convertTypeFunArgs [] = return []
convertTypeFunArgs (arg:argt) = do
  rest <- convertTypeFunArgs argt
  res <- convertTypeVar arg
  return $ res : rest

getVar :: L.Ident -> L.BNFC'Position -> IM TVar
getVar (L.Ident ident) pos = do
  e <- ask
  let maybe_var = M.lookup (L.Ident ident) (vars e) in
    case maybe_var of
      Nothing -> throwError $ Err $ "Use of an undeclared variable " ++ ident ++ displayPos pos
      Just var -> return var

getFunc :: L.Ident -> L.BNFC'Position -> IM FunType
getFunc (L.Ident ident) pos = do
  s <- get
  let maybe_func = M.lookup (L.Ident ident) (functions s) in
    case maybe_func of
      Nothing -> throwError $ Err $ "Use of an undeclared function " ++ ident ++ " " ++ displayPos pos
      Just func -> return func

readFunDecls :: [L.TopDef' L.BNFC'Position] -> IM ()
readFunDecls [] = return ()
readFunDecls ((L.FnDef pos ftype fname fargs fbody):ft) = do
  s <- get
  when (M.member fname (functions s)) (throwError $ Err $ "Duplicate function name " ++ displayName fname ++ " " ++ displayPos pos)
  farg_types <- convertTypeFunArgs fargs
  fun_type <- convertType ftype
  put $ Store $ M.insert fname (Fun fname farg_types fun_type) (functions s)
  readFunDecls ft

assertType pos type_ expr message = do
  expr_type <- evalType expr
  when (type_ /= expr_type) (throwError $ Err $ message ++ " expected: " ++ show type_
   ++ ", received: " ++ show expr_type ++ " " ++ displayPos pos)
  return type_

evalBinMulOp :: L.BNFC'Position -> L.Expr' L.BNFC'Position -> L.Expr' L.BNFC'Position -> L.MulOp' L.BNFC'Position -> IM Objtype
evalBinMulOp pos a b op = do
  assertType pos TInt a $ "Cannot perform binary operation " ++ show op ++ " because of the left-hand argument - "
  assertType pos TInt b $ "Cannot perform binary operation " ++ show op ++ " because of the right-hand argument - "
  return TInt

evalBinAddOp :: L.BNFC'Position -> L.Expr' L.BNFC'Position -> L.Expr' L.BNFC'Position -> L.AddOp' L.BNFC'Position -> IM Objtype
evalBinAddOp pos a b (L.Minus _) = do
  assertType pos TInt a $ "Cannot perform binary operation " ++ "Minus" ++ " because of the left-hand argument - "
  assertType pos TInt b $ "Cannot perform binary operation " ++ "Minus" ++ " because of the right-hand argument - "
  return TInt

evalBinAddOp pos a b (L.Plus _) = do
  atype <- evalType a
  btype <- evalType b
  when (atype /= btype) $ throwError $ Err $ "Cannot perform binary operation Plus because of type mismatch - " ++
    show atype ++ " is not equal to " ++ show btype ++ " " ++ displayPos pos
  checkValidTypes atype
  where
    checkValidTypes :: Objtype -> IM Objtype
    checkValidTypes TInt = return TInt
    checkValidTypes TString = return TString
    checkValidTypes atype = throwError $ Err $ "Cannot perform binary operation Plus on type "
      ++ show atype ++ displayPos pos

evalBinRelOpAllTypes pos a b = do
  atype <- evalType a
  assertType pos atype b "Cannot compare types:"
  return TBool

evalBinRelOp pos a b (L.EQU _) = do
  evalBinRelOpAllTypes pos a b
  return TBool

evalBinRelOp pos a b (L.NE _) = do
  evalBinRelOpAllTypes pos a b

evalBinRelOp pos a b op = do
  assertType pos TInt a $ "Cannot compare given type with operation: " ++ show op
  assertType pos TInt b $ "Cannot compare given type with operation: " ++ show op
  return TBool

evalType :: L.Expr' L.BNFC'Position -> IM Objtype
evalType (L.EVar pos x) = do
  (TVar _ var_type) <- getVar x pos
  return var_type

evalType (L.ELitInt pos x) = return TInt

evalType (L.ELitTrue pos) = return TBool

evalType (L.ELitFalse pos) = return TBool

evalType (L.EApp pos fident args) = do
  (Fun _ fargs ftype) <- getFunc fident pos
  verifyArgsList fargs args pos
  return ftype
  where
    verifyArgsList ((TVar _ farg_type):fargs) (arg:args) pos = do
      assertType pos farg_type arg "Invalid argument given to function:"
      verifyArgsList fargs args pos
    verifyArgsList [] [] pos = return ()
    verifyArgsList al [] pos = throwError $ Err $ "Invalid arguments count at function application " ++ displayPos pos
      ++ " - expected " ++ show (length al) ++ " more"
    verifyArgsList [] rl pos = throwError $ Err $ "Invalid arguments count at function application " ++ displayPos pos
      ++ " - expected " ++ show (length rl) ++ " less"

evalType (L.EString _ _) = return TString

evalType (L.Neg pos expr) = do
  assertType pos TInt expr "Cannot negate type - "
  return TInt

evalType (L.Not pos expr) = do
  assertType pos TBool expr "Cannot perform NOT on type - "
  return TBool

evalType (L.EMul pos a op b) = do
  evalBinMulOp pos a b op

evalType (L.EAdd pos a op b) = do
  evalBinAddOp pos a b op

evalType (L.ERel pos a op b) = do
  evalBinRelOp pos a b op

evalType (L.EAnd pos a b) = do
  assertType pos TBool a "Wrong type to logic AND operator - "
  assertType pos TBool b "Wrong type to logic AND operator - "
  return TBool

evalType (L.EOr pos a b) = do
  assertType pos TBool a "Wrong type to logic OR operator - "
  assertType pos TBool b "Wrong type to logic OR operator - "
  return TBool

checkTypesMatch decl_type expr pos errorMessage = do
  expr_type <- evalType expr
  when (expr_type /= decl_type) (throwError $ Err $
     errorMessage ++ " " ++ displayPos pos ++ " - expected: " ++ show decl_type
      ++ ", calculated: " ++ show expr_type
    )

verifyDeclarations :: L.Type' L.BNFC'Position -> [L.Item' L.BNFC'Position] -> IM Env
verifyDeclarations (L.Void pos) _ = throwError $ Err $ "Cannot declare variable of type void - error " ++ displayPos pos
verifyDeclarations (L.Fun pos _ _) _ = throwError $ Err $ "Cannot declare variable-function - error " ++ displayPos pos
verifyDeclarations _ [] = do ask
verifyDeclarations t ((L.NoInit pos ident):dt) = do
  var <- convertTypeVar (L.Arg pos t ident)
  e <- ask
  when (M.member ident $ currentBlockVars e) $
    throwError $ Err $ "Variable " ++ displayName ident ++ " redeclared within block " ++ displayPos pos
  local (const $ Env (M.insert ident var $ vars e)
    (M.insert ident var $ currentBlockVars e) (currentRet e)) $ verifyDeclarations t dt
verifyDeclarations t ((L.Init pos ident expr):dt) = do
  decl_type <- convertType t
  checkTypesMatch decl_type expr pos "Types mismatch in variable declaration"
  verifyDeclarations t (L.NoInit pos ident:dt)

verifyStatement :: L.Stmt' L.BNFC'Position -> IM Env
verifyStatement (L.Empty _) = do ask

verifyStatement (L.BStmt _ (L.Block _ stmts)) = do
  e <- ask
  local (const $ Env (vars e) M.empty (currentRet e)) $ verifyBlock stmts
  return e

verifyStatement (L.Decl _ type_ idents) = do
  verifyDeclarations type_ idents

verifyStatement (L.Ass pos ident expr) = do
  var <- getVar ident pos
  checkTypesMatch (varType var) expr pos "Types mismatch in variable assignment"
  ask

verifyStatement (L.Incr pos ident) = do
  var <- getVar ident pos
  checkTypesMatch (varType var) (L.ELitInt pos 0) pos "Error while trying to increment a non-integer variable"
  ask

verifyStatement (L.Decr pos ident) = do
  var <- getVar ident pos
  checkTypesMatch (varType var) (L.ELitInt pos 0) pos "Error while trying to decrement a non-integer variable"
  ask

verifyStatement (L.Ret pos expr) = do
  e <- ask
  let current_ret = currentRet e in
    checkTypesMatch current_ret expr pos $ "Invalid return type " ++ displayPos pos
  return e

verifyStatement (L.VRet pos) = do
  e <- ask
  when (currentRet e /= TVoid) (throwError $ Err $
    "Trying to return void in a function of type: " ++ show (currentRet e) ++ " " ++ displayPos pos)
  return e

verifyStatement (L.Cond pos cond st) = do
  checkTypesMatch TBool cond pos "If condition has to be of type Bool"
  verifyStatement st

verifyStatement (L.CondElse pos cond stt stf) = do
  checkTypesMatch TBool cond pos "If condition has to be of type Bool"
  verifyStatement stt
  verifyStatement stf

verifyStatement (L.While pos cond st) = do
  checkTypesMatch TBool cond pos "While condition has to be of type Bool"
  verifyStatement st

verifyStatement (L.SExp pos expr) = do
  evalType expr
  ask

verifyBlock :: [L.Stmt' L.BNFC'Position] -> IM ()
verifyBlock [] = return()
verifyBlock (st:stt) = do
  e <- verifyStatement st
  local (const e) $ verifyBlock stt

verifyBlocks :: [L.TopDef' L.BNFC'Position] -> Either FrontendError () -> IM ()
verifyBlocks _ (Left err) = throwError err
verifyBlocks [] _ = return ()
verifyBlocks ((L.FnDef pos ftype fname fargs (L.Block _ fbody)):ft) err = do
  ftype_conv <- convertType ftype
  fargs_conv <- convertTypeFunArgs fargs
  e_with_fargs <- addArgs fargs_conv
  local (\e -> Env (vars e_with_fargs) M.empty ftype_conv) $ verifyBlock fbody
  verifyBlocks ft err
  where
    addArgs :: [TVar] -> IM Env
    addArgs ((TVar ident type_):args) = do
      e <- ask
      when (M.member ident $ vars e) $ throwError $ Err $ "Duplicate argument name " ++ displayName ident ++ " " ++ displayPos pos
      local (\e -> Env (M.insert ident (TVar ident type_) (vars e)) M.empty (currentRet e)) $ addArgs args
    addArgs [] = ask

verifyReturnPresentSt :: L.Stmt' L.BNFC'Position -> Bool -> IM Bool
verifyReturnPresentSt (L.Ret _ _ ) _ = return True
verifyReturnPresentSt (L.VRet _) _ = return True
verifyReturnPresentSt (L.BStmt pos (L.Block _ b)) old = do
  b_reached <- verifyReturnsPresentBlock b
  return $ b_reached || old
verifyReturnPresentSt (L.Cond _ (L.ELitTrue _) b) old = verifyReturnPresentSt b True
verifyReturnPresentSt (L.Cond _ _ b) old = return old
verifyReturnPresentSt (L.CondElse _ (L.ELitFalse _) _ b) old = do
  verifyReturnPresentSt b old
verifyReturnPresentSt (L.CondElse _ (L.ELitTrue _) b _) old = do
  verifyReturnPresentSt b old
verifyReturnPresentSt (L.CondElse _ _ a b) old = do
  verifyReturnPresentSt b old
  verifyReturnPresentSt a old
verifyReturnPresentSt (L.While _ (L.ELitFalse _) _) old = return old
verifyReturnPresentSt (L.While _ (L.ELitTrue _) _) old = return True
verifyReturnPresentSt (L.While _ _ b) old = verifyReturnPresentSt b old
verifyReturnPresentSt e old = return old

verifyReturnsPresentBlock :: [L.Stmt' L.BNFC'Position] -> IM Bool
verifyReturnsPresentBlock [] = return False
verifyReturnsPresentBlock (st:stt) = verifyReturnsPresentBlock stt >>= verifyReturnPresentSt st

verifyReturnsPresentBlocks :: [L.TopDef' L.BNFC'Position] -> Either FrontendError () -> IM ()
verifyReturnsPresentBlocks _ (Left err) = throwError err
verifyReturnsPresentBlocks [] _ = return ()
verifyReturnsPresentBlocks ((L.FnDef pos ftype fname fargs (L.Block _ fbody)):ft) err = do
  ftype_conv <- convertType ftype
  case ftype_conv of
    TVoid -> return ()
    _ -> do
      returnReached <- verifyReturnsPresentBlock fbody
      unless returnReached $ throwError $ Err $ "No return found in function: " ++ displayName fname ++ " declared " ++ displayPos pos
  verifyReturnsPresentBlocks ft err

checkMainPresent :: Store -> Either FrontendError () -> ExceptT FrontendError IO ()
checkMainPresent _ (Left err) = throwError err
checkMainPresent store _ = do
  unless (M.member (L.Ident "main") (functions store)) $ throwError $ Err "Function 'main' not found!"

runFrontend :: L.Program -> IO (Either FrontendError ())
runFrontend (L.Program _ prog) = do
  (_, store) <- runReaderT (runStateT (runExceptT addPredefinedFunctions) $ Store M.empty) $ Env M.empty M.empty TVoid
  (err, store) <- runReaderT (runStateT (runExceptT $ readFunDecls prog) store) $ Env M.empty M.empty TVoid
  err <- runExceptT $ checkMainPresent store err
  (err, _) <- runReaderT (runStateT (runExceptT $ verifyBlocks prog err) store) $ Env M.empty M.empty TVoid
  (err, _) <- runReaderT (runStateT (runExceptT $ verifyReturnsPresentBlocks prog err) store) $ Env M.empty M.empty TVoid
  return err