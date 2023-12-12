module Compiler where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad (when)
import Data.Maybe(fromMaybe, isNothing)
import qualified Data.Map as M
import qualified AbsLatte as L
import CFG
import IR
import qualified PropagateConstants as PR
import qualified FoldConstants as FC
import System.IO (hPrint, stderr)
import System.Exit (exitWith, ExitCode (ExitFailure), exitFailure)

convertFuncsToIR :: FuncTypesMap -> [L.TopDef' L.BNFC'Position] -> IO ()
convertFuncsToIR _ [] = return ()
convertFuncsToIR ftm ((L.FnDef pos ftype (L.Ident fname) fargs (L.Block _ fbody)):ft) = do
  let fname_modded = modifyFname fname
  let blabel = Blabel fname_modded
  let (farg_topdefs, earg_map, vars_map, vartypes_map, inhvars_lst) = parseFargs fargs
  let bmp = M.singleton blabel $ CFGBlock blabel None [] [] vars_map vartypes_map inhvars_lst 
  ((), store) <- runReaderT (runStateT (convertFuncToIR fbody) (Store 1 1 blabel bmp ftm [])) $ Env earg_map
  printFtop (show $ mapType ftype) fname_modded farg_topdefs
  --((), _) <- runStateT (printBlocks (blocks store) True) (BFSOutput [] [blabel])
  
  --liftIO $ putStr "versus: \n\n"
  cfgblocks <- optimize blabel $ blocks store
  ((), _) <- runStateT (printBlocks cfgblocks True) (BFSOutput [] [blabel])
  liftIO $ putStrLn "}"
  convertFuncsToIR ftm ft
  
-- FIXME uporządkuj bałagan w kolejności funkcji

optimize :: Blabel -> M.Map Blabel CFGBlock -> IO CFGBMap
optimize entryBlabel storeBlocks = do
  (foundP, store) <- runStateT (PR.propagateConstants entryBlabel) $ PR.PropagateStore storeBlocks False (Llabel "dummy") ILitFalse 0 False []
  (foundF, fblocks) <- FC.foldConstants $ PR.blockMap store
  if foundF || foundP then
    optimize entryBlabel fblocks
  else
    return fblocks

-- vars :: M.Map Llabel Llabel,
--     varTypes :: M.Map Llabel LabelType,
--     inheritedVars :: [Llabel]

parseFargs :: [L.Arg' a] -> ([String], M.Map L.Ident Llabel, M.Map Llabel Llabel, M.Map Llabel LabelType, [Llabel])
parseFargs [] = ([], M.empty, M.empty, M.empty, [])
parseFargs ((L.Arg _p t (L.Ident x)):xt) =
  let (lst, emp, vars, vartypes, inhvars) = parseFargs xt in
  let xlabel = Llabel $ "var_" ++ x in
    let 
      emp_x = M.insert (L.Ident x) xlabel emp
      vars_x = M.insert xlabel xlabel vars 
      vartypes_x = M.insert xlabel (matchT t) vartypes
      inhvars_x = xlabel : inhvars in
    ((show (matchT t) ++ " " ++ show xlabel) : lst, emp_x, vars_x, vartypes_x, inhvars_x)
  where 
  matchT (L.Int _) = LI32
  matchT (L.Bool _) = LI1
  matchT (L.Str _) = LI8

printFtop :: String -> String -> [String] -> IO ()
printFtop ftype fname fargs = do
  liftIO $ putStrLn $ "define " ++ ftype ++ " @" ++ fname ++ "(" ++ printArgs fargs ++ ") {"
  where
  printArgs [] = ""
  printArgs (a:at) = a ++ (if null at then "" else ", ") ++ printArgs at

printBlocks :: M.Map Blabel CFGBlock -> Bool -> OutputIM ()
printBlocks blocks suppressFirstName = do
  s <- get
  if null $ to_visit s then return () else do
  let (current : remaining) = to_visit s
  put $ s {to_visit = remaining, visited = current : visited s}
  curB <- getFromMapOrErr2 current blocks
  let (Blabel blabel) = label curB
  --unless suppressFirstName $ 
  liftIO $ putStrLn $ blabel ++ ":"
  if null $ quads curB then liftIO $ putStrLn "ret void"
    else displayQuads $ quads curB
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
    liftIO $ putStr $ show q

addPredefinedFunctions :: FuncTypesMap
addPredefinedFunctions = M.fromList [
    (L.Ident "printInt", (LVoid, [LI32])),
    (L.Ident "printString", (LVoid, [LI8])),
    (L.Ident "error", (LVoid, [])),
    (L.Ident "readInt", (LI32, [])),
    (L.Ident "readString", (LI8, []))
  ]

addProgFunctions :: [L.TopDef' L.BNFC'Position] -> FuncTypesMap
addProgFunctions [] = M.empty
addProgFunctions ((L.FnDef pos ftype (L.Ident fname) fargs (L.Block _ fbody)):ft) = 
  let 
    rest = addProgFunctions ft 
    fnameModded = modifyFname fname in
    M.insert (L.Ident fname) (mapType ftype, map (\(L.Arg _ t _) -> mapType t) fargs) rest

compile :: L.Program' L.BNFC'Position -> IO ()
compile (L.Program _ prog) = do
  liftIO $ putStrLn "declare void @printInt(i32)"
  liftIO $ putStrLn "declare void @printString(i8*)"
  liftIO $ putStrLn "declare void @error()"
  liftIO $ putStrLn "declare i32 @readInt()"
  liftIO $ putStrLn "declare i8* @readString()"
  let funcTypes = M.union addPredefinedFunctions $ addProgFunctions prog
  convertFuncsToIR funcTypes prog
  return ()
