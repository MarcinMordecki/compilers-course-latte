{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IR where

import qualified AbsLatte as L

import Control.Monad.State

import qualified Data.String

newtype Llabel = Llabel String
  deriving (Eq, Ord, Show, Read, Data.String.IsString)

newtype Blabel = Blabel String
  deriving (Eq, Ord, Show, Read, Data.String.IsString)

data IExprSimple
  = ILabel Llabel
  | IReplaceVar L.Ident
  | IBr0 Blabel
  | IBrCond Llabel Blabel Blabel
  | IPhi L.Ident [(Blabel, Llabel)] 
  | ILitInt Integer
  | ILitTrue
  | ILitFalse
  | ILitString String
  | IApp L.Ident [Llabel]
  deriving (Eq)

instance Show IExprSimple where
  show (ILabel (Llabel x)) = "%" ++ x
  show (IReplaceVar x) = show x
  show (IBr0 x) = "br " ++ show x
  show (IBrCond c (Blabel a) (Blabel b)) = "br " ++ show c ++ " " ++ a ++ ", " ++ b
  show (IPhi _ x) = "%phi" ++ show x
  show (ILitInt x) = show x
  show ILitTrue = "true"
  show ILitFalse = "false"
  show (ILitString x) = x
  show (IApp (L.Ident f) s) = f ++ "(" ++ show s ++ ")"

data IExpr'
  = Simple IExprSimple
  | INeg IExprSimple
  | INot IExprSimple
  | IMul L.MulOp Llabel Llabel
  | IAdd L.AddOp Llabel Llabel
  | IRel L.RelOp Llabel Llabel
  | IAnd Llabel Llabel
  | IOr Llabel Llabel
  | IRet IExprSimple
  | IVRet
  deriving (Eq)
instance Show IExpr' where
  show (Simple x) = show x
  show (INeg x) = "-" ++ show x
  show (INot x) = "!" ++ show x
  show (IMul o a b) = show o ++ " " ++ show a ++ ", " ++ show b
  show (IAdd o a b) = show o ++ " " ++ show a ++ ", " ++ show b
  show (IRel o a b) = show o ++ " " ++ show a ++ ", " ++ show b
  show (IAnd a b) = show a ++ " && " ++ show b
  show (IOr a b) = show a ++ " || " ++ show b
  show (IRet e) = "ret i32 " ++ show e
  show IVRet = "ret"

data IExpr 
  = IExpr {
    lhs :: Llabel,
    expr :: IExpr'
  }
  deriving (Eq)
instance Show IExpr where
  show (IExpr (Llabel lhs) expr) = "%" ++ lhs ++ " = " ++ show expr
