module FoldConstants where

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

foldConstants :: M.Map Blabel CFGBlock -> IO (Bool, M.Map Blabel CFGBlock)
foldConstants b = do
  let b' = M.map (\block -> block {quads = map foldC $ quads block}) b
  return (b /= b', b')

foldC :: IExpr -> IExpr 
foldC (IExpr l (INot ILitFalse)) = IExpr l $ Simple ILitTrue
foldC (IExpr l (INot ILitTrue)) = IExpr l $ Simple ILitFalse
foldC (IExpr l (IBinOp op (ILitInt a) (ILitInt b))) = IExpr l $ Simple $ applyIntOp op a b
  where 
  applyIntOp IPlus a b = ILitInt $ a + b
  applyIntOp IMinus a b = ILitInt $ a - b
  applyIntOp IMul a b = ILitInt $ a * b
  applyIntOp IDiv a b = ILitInt $ a `div` b
  applyIntOp IMod a b = ILitInt $ a `mod` b
  applyIntOp IEq a b = if a == b then ILitTrue else ILitFalse
  applyIntOp INe a b = if a /= b then ILitTrue else ILitFalse
  applyIntOp IGe a b = if a >= b then ILitTrue else ILitFalse
  applyIntOp IGt a b = if a > b then ILitTrue else ILitFalse
  applyIntOp ILe a b = if a <= b then ILitTrue else ILitFalse
  applyIntOp ILt a b = if a < b then ILitTrue else ILitFalse
foldC (IExpr l (IBinOp IEq ILitTrue ILitTrue)) = IExpr l $ Simple ILitTrue
foldC (IExpr l (IBinOp IEq ILitFalse ILitFalse)) = IExpr l $ Simple ILitTrue
foldC (IExpr l (IBinOp IEq ILitTrue ILitFalse)) = IExpr l $ Simple ILitFalse
foldC (IExpr l (IBinOp IEq ILitFalse ILitTrue)) = IExpr l $ Simple ILitFalse
foldC (IExpr l (IBinOp INe ILitTrue ILitTrue)) = IExpr l $ Simple ILitFalse
foldC (IExpr l (IBinOp INe ILitFalse ILitFalse)) = IExpr l $ Simple ILitFalse
foldC (IExpr l (IBinOp INe ILitTrue ILitFalse)) = IExpr l $ Simple ILitTrue
foldC (IExpr l (IBinOp INe ILitFalse ILitTrue)) = IExpr l $ Simple ILitTrue
foldC x = x