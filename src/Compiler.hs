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


data BFSOutput
  = BFSOutput {
    visited :: [Blabel],
    to_visit :: [Blabel]
  }

type OutputIM a = StateT BFSOutput IO a


getFromMapOrErr2 :: Ord a => a -> M.Map a b -> OutputIM b
getFromMapOrErr2 key map = do
  let v = M.lookup key map in
    case v of
      Nothing -> liftIO $ print "Error that should never happen unless compilator is faulty\n"
        >> exitWith (ExitFailure 1)
      Just x -> return x


convertFuncsToIR :: [L.TopDef' L.BNFC'Position] -> IO ()
convertFuncsToIR [] = return ()
convertFuncsToIR ((L.FnDef pos ftype (L.Ident fname) fargs (L.Block _ fbody)):ft) = do
  let blabel = Blabel $ fname ++ "_entry"
  let (farg_names, farg_map) = parseFargs fargs
  let bmp = M.singleton blabel $ CFGBlock blabel None [] [] M.empty
  ((), store) <- runStateT (convertFuncToIR fbody) (Store 1 1 blabel bmp)
  printFtop (fname ++ "_entry") farg_names
  ((), _) <- runStateT (printBlocks (blocks store) True) (BFSOutput [] [blabel])
  liftIO $ putStrLn "}"
  liftIO $ putStr "versus: \n\n"
  cfgblocks <- optimize $ blocks store
  ((), _) <- runStateT (printBlocks cfgblocks True) (BFSOutput [] [blabel])
  liftIO $ putStr "\n\n\n\n"
  convertFuncsToIR ft
  
-- FIXME uporządkuj bałagan w kolejności funkcji

optimize :: M.Map Blabel CFGBlock -> IO CFGBMap
optimize storeBlocks = do
  (foundP, store) <- runStateT PR.propagateConstants $ PR.PropagateStore storeBlocks False (Llabel "dummy") ILitFalse 0 False
  (foundF, fblocks) <- FC.foldConstants $ PR.blockMap store
  if foundF || foundP then
    optimize fblocks
  else
    return fblocks

parseFargs :: [L.Arg' a] -> ([String], M.Map L.Ident Llabel)
parseFargs [] = ([], M.empty)
parseFargs ((L.Arg _ t x):xt) =
  let (lst, mp) = parseFargs xt in
  let xlabel = Llabel $ "%var_" ++ show x in
    let mp_x = M.insert x xlabel mp in
    ((matchT t ++ show xlabel) : lst, mp_x)
  where 
  matchT (L.Int _) = "int32 "
  matchT (L.Bool _) = "int1 "
  matchT (L.Str _) = "i8* "

printFtop :: String -> [String] -> IO ()
printFtop fname fargs = do
  liftIO $ putStrLn $ "define i32 @" ++ fname ++ "(" ++ printArgs fargs ++ ") {"
  where
  printArgs [] = ""
  printArgs (a:at) = a ++ ", " ++ printArgs at

printBlocks :: M.Map Blabel CFGBlock -> Bool -> OutputIM ()
printBlocks blocks suppressFirstName = do
  s <- get
  if null $ to_visit s then return () else do
  let (current : remaining) = to_visit s
  put $ s {to_visit = remaining, visited = current : visited s}
  curB <- getFromMapOrErr2 current blocks
  unless suppressFirstName $ liftIO $ putStrLn $ show (label curB) ++ ":"
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
    liftIO $ putStr $ show q

compile :: L.Program' L.BNFC'Position -> IO ()
compile (L.Program _ prog) = do
  convertFuncsToIR prog
  return ()