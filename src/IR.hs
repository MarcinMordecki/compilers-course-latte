{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IR where

import qualified AbsLatte as L

import Control.Monad.State

import qualified Data.String

newtype Llabel = Llabel String
  deriving (Eq, Ord, Read, Data.String.IsString)
instance Show Llabel where
  show (Llabel l) = "%" ++ l

newtype Blabel = Blabel String
  deriving (Eq, Ord, Read, Data.String.IsString)
instance Show Blabel where
  show (Blabel b) = "%" ++ b

data IExprSimple
  = ILabel Llabel
  | ILitInt Integer
  | ILitTrue
  | ILitFalse
  deriving (Eq)

instance Show IExprSimple where
  show (ILabel (Llabel x)) = "%" ++ x
  show (ILitInt x) = show x
  show ILitTrue = "true"
  show ILitFalse = "false"

data ITBinOp 
  = IPlus
  | IMinus
  | IMul
  | IDiv
  | IMod
  | IEq
  | INe
  | IGe
  | IGt
  | ILe
  | ILt
  deriving (Eq)
instance Show ITBinOp where
  show IPlus = "add i32"
  show IMinus = "sub i32"
  show IMul = "mul i32"
  show IDiv = "sdiv i32"
  show IMod = "smod i32"
  show IEq = "icmp eq"
  show INe = "icmp ne"
  show IGe = "icmp ge"
  show IGt = "icmp gt"
  show ILe = "icmp le"
  show ILt = "icmp lt"

data IExpr'
  = Simple IExprSimple
  | IBr0 Blabel 
  | IBrCond Llabel Blabel Blabel
  | IPhi L.Ident [(Blabel, IExprSimple)] 
  | IAOrPhi [(Blabel, IExprSimple)] 
  | IApp L.Ident [IExprSimple]
  | INeg IExprSimple
  | INot IExprSimple
  | IBinOp ITBinOp IExprSimple IExprSimple
  | IStrPlus IExpr'
  | IAnd IExprSimple IExprSimple
  | IOr IExprSimple IExprSimple
  | IRet IExprSimple
  | IVRet
  | ILitString String
  deriving (Eq)
instance Show IExpr' where
  show (Simple x) = show x
  show (IBr0 x) = "br " ++ show x
  show (IBrCond c (Blabel a) (Blabel b)) = "br " ++ show c ++ " " ++ a ++ ", " ++ b
  show (IPhi _ x) = "%phi" ++ show x
  show (IAOrPhi x) = "%cphi" ++ show x
  show (IApp (L.Ident f) s) = f ++ "(" ++ show s ++ ")"
  show (INeg x) = "-" ++ show x
  show (INot x) = "!" ++ show x
  show (IBinOp o a b) = show o ++ " " ++ show a ++ ", " ++ show b
  show (IAnd a b) = show a ++ " && " ++ show b
  show (IOr a b) = show a ++ " || " ++ show b
  show (IRet e) = "ret i32 " ++ show e
  show IVRet = "ret"
  show (ILitString x) = "FIXME ADD STRINGS!!!1 " ++ x

data IExpr 
  = IExpr {
    lhs :: Llabel,
    expr :: IExpr'
  }
  | KilledIExpr
  deriving (Eq)
instance Show IExpr where
  show (IExpr (Llabel lhs) expr) = "%" ++ lhs ++ " = " ++ show expr ++ "\n"
  show KilledIExpr = ""
