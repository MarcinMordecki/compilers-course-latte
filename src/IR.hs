{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IR where

import qualified AbsLatte as L
import Control.Monad.State
import qualified Data.String
import qualified Data.Map as M
import System.Exit (exitWith, ExitCode (ExitFailure), exitFailure)

data LabelType
  = LI1
  | LI8
  | LI32
  | LVoid
  deriving (Eq)
instance Show LabelType where
  show LI1 = "i1"
  show LI8 = "i8*"
  show LI32 = "i32"
  show LVoid = "void"

data Llabel = Llabel String
  deriving (Eq, Ord, Read)
instance Show Llabel where
  show (Llabel l) = "%" ++ l

newtype Slabel = Slabel String
  deriving (Eq, Ord, Read, Data.String.IsString)
instance Show Slabel where
  show (Slabel l) = "@" ++ l

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
  show IMod = "srem i32"
  show IEq = "icmp eq"
  show INe = "icmp ne"
  show IGe = "icmp sge i32"
  show IGt = "icmp sgt i32"
  show ILe = "icmp sle i32"
  show ILt = "icmp slt i32"

mapType :: L.Type -> LabelType
mapType (L.Int _) = LI32
mapType (L.Bool _) = LI1
mapType (L.Str _) = LI8
mapType (L.Void _) = LVoid

data IExpr'
  = Simple IExprSimple
  | IBr0 Blabel
  | IBrCond Llabel Blabel Blabel
  | IPhi Llabel LabelType [(Blabel, IExprSimple)]  -- Llabel of first declaration
  | IAOrPhi [(Blabel, IExprSimple)]
  | IApp L.Ident LabelType [LabelType] [IExprSimple]
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
  show (IBr0 x) = "br label " ++ show x
  show (IBrCond c a b) = "br i1 " ++ show c ++ ", label " ++ show a ++ ", label " ++ show b
  show (IPhi _ t x) = "phi " ++ show t ++ displayPhiList x
  show (IAOrPhi x) = show $ IPhi (Llabel "") LI1 x
  show (IApp (L.Ident f) t stype s) = "call " ++ show t ++ " @" ++ f ++ "(" ++ displayArgs (zip stype s) ++ ")"
  show (INeg x) = show (IBinOp IMinus (ILitInt 0) x)
  show (INot x) = "xor i1 1, " ++ show x
  show (IBinOp o a b) = show o ++ " " ++ show a ++ ", " ++ show b
  show (IAnd a b) = show a ++ " && " ++ show b
  show (IOr a b) = show a ++ " || " ++ show b
  show (IRet e) = "ret i32 " ++ show e
  show IVRet = "ret void"
  show (ILitString x) = "FIXME ADD STRINGS!!!1 " ++ x

reservedFuncs = ["printInt", "printString", "error", "readInt", "readString", "main"]

modifyFname :: String -> String
modifyFname fname = if fname `elem` reservedFuncs then fname else fname ++ "_entry"  -- FIXME think it over

display :: String -> IExpr' -> String
display lhs (IBr0 x) = show $ IBr0 x
display lhs (IBrCond c a b) = show $ IBrCond c a b
display lhs (IRet e) = show $ IRet e
display lhs IVRet = show IVRet
display lhs (IApp name typ argtyp args) = (if typ == LVoid then "" else "%" ++ lhs ++ " = ") ++ show (
  IApp ((\(L.Ident n) -> L.Ident $ modifyFname n) name) typ argtyp args)
display lhs e = "%" ++ lhs ++ " = " ++ show e

displayType :: IExprSimple -> String
displayType (ILitInt _) = "i32"
displayType ILitTrue = "i1"
displayType ILitFalse = "i1"
displayType (ILabel _) = "i69"

displayArgs :: [(LabelType, IExprSimple)] -> String
displayArgs [] = ""
displayArgs ((atype, a):at) = show atype ++ " " ++ show a ++ (if null at then "" else ", ") ++ displayArgs at

displayPhiList :: [(Blabel, IExprSimple)] -> String
displayPhiList [] = ""
displayPhiList ((b, l):phit) = "[" ++ show l ++ "," ++ show b ++ "]" ++ (if null phit then "" else ", " ) ++ displayPhiList phit

data IExpr
  = IExpr {
    lhs :: Llabel,
    expr :: IExpr'
  }
  | KilledIExpr
  deriving (Eq)
instance Show IExpr where
  show (IExpr (Llabel lhs) expr) = display lhs expr ++ "\n"
  show KilledIExpr = ""

data IStringConstant
  = IStringConstant {
    slabel :: Slabel,
    len :: Int,
    content :: String
  }
  deriving (Eq)
instance Show IStringConstant where
  show (IStringConstant sl le cont) = show sl ++ " = private constant [" ++ show le ++ " x i8] c" ++ show cont   

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