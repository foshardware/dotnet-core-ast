
module Language.Net.Core.CSharp.Syntax where

import Data.Text (Text)

data AST = AST [File]
  deriving (Eq, Show)

data File = File [Import] [Namespace]
  deriving (Eq, Show)
data Import = Import Name (Maybe Name)
  deriving (Eq, Show)
data Namespace = Namespace Name [TypeDec]
  deriving (Eq, Show)

data TypeDec = TypeDec Attr TypeDef
  deriving (Eq, Show)
data TypeDef = ClassDef ClassDec | EnumDef EnumDec
  deriving (Eq, Show)

data MemberDec = MemberDec Attr MemberDef
  deriving (Eq, Show)
data MemberDef
  = MemberClass ClassDec
  | MemberEnum  EnumDec 
  | Member TypeIdent (Either MethodDec FieldDec)
  deriving (Eq, Show)

data ClassDec = ClassDec Module Type [Type] [TypeInheritance] [MemberDec]
  deriving (Eq, Show)
data EnumDec = EnumDec Type [Text]
  deriving (Eq, Show)
data MethodDec = MethodDec [Arg] [Expr] [TypeInheritance] [Stmt]
  deriving (Eq, Show)
data FieldDec = FieldDec (Maybe FieldTransform) (Maybe Expr)
  deriving (Eq, Show)

type TypeInheritance = (Type, Type)

data TypeIdent = TypeIdent Type (Maybe Text)
  deriving (Eq, Show)

data Attr = Attr [TypeAttr] (Maybe AccessMod) [TypeMod]
  deriving (Eq, Show)

data Variant a = Covariant a | Contravariant a | Invariant a
  deriving (Eq, Show)

type TypeAttr = [Expr]

data FieldTransform = FieldTransform (Maybe (Either Expr [Stmt])) (Maybe (Either Expr [Stmt]))
  deriving (Eq, Show)

data Arg = Arg [TypeAttr] Type (Maybe ArgMod) (Maybe Text) (Maybe Expr)
  deriving (Eq, Show)

data ArgMod
  = Array (Maybe Expr)
  deriving (Eq, Show)

type Name = [Text]

data Stmt
  = EmptyStmt
  | IfStmt Expr [Stmt] [Stmt]
  | CaseStmt Expr [(Expr, [Stmt])] (Maybe [Stmt])
  | UsingStmt Expr [Stmt]
  | Catch [Stmt] [(Arg, [Stmt])]
  | ForLoop (Maybe Expr) (Maybe Expr) (Maybe Expr) [Stmt]
  | ForEachLoop Arg Expr [Stmt]
  | Return (Maybe Expr)
  | Break | Continue
  | Assignment Expr Expr
  | Exception Expr 
  | Call Expr
  | Stmt Expr
  deriving (Eq, Show)

data Expr
  = Bind Type Expr -- variable binding
  | This | Base
  | Int | Double | String
  | Point Text
  | MaybeNull Text
  | LitString Text
  | LitVerbatim Text
  | LitInt Int
  | LitReal Text
  | LitChar Char
  | LitBool Bool
  | TypeOf Type
  | Glue Expr
  | Null
  | Dynamic
  | Op Operator
  | Ex Expr (Maybe Expr) 
  | Ty Type
  deriving (Eq, Show)

data Operator
  = PreIncrement Text
  | PreDecrement Text
  | PostIncrement Text
  | PostDecrement Text
  | Await Expr
  | MethodCall [Expr] (Maybe Operator)
  | ArrayAccess Expr (Maybe Operator)
  | NullArrayAccess Expr (Maybe Operator)
  | Lambda (Either [Stmt] Expr)
  | Or Expr | And Expr
  | Assign Expr
  | Not Expr
  | Access Expr
  | NullAccess Expr
  | RecordAccess [Expr]
  | Plus Expr | Minus Expr | Mult Expr | Div Expr | Modulo Expr
  | Eq Expr | NotEq Expr
  | Less Expr | Greater Expr | LessOrEqual Expr | GreaterOrEqual Expr
  | Out Expr | In Expr
  | Cast Type
  | If Expr Expr
  deriving (Eq, Show)

data Type
  = Identity Module
  | PrimitiveType TypeEnum (Maybe ArgMod)
  | Type Text (Maybe ArgMod)
  | HigherOrder Text [Variant Type] (Maybe ArgMod)
  | ArrayType Text
  | Nullable Text
  | NullType
  deriving (Eq, Show)

data Module = ModClass | ModInterface | ModDelegate
  deriving (Eq, Show)

data TypeEnum
  = PBool
  | PByte
  | PSbyte
  | PChar
  | PDecimal
  | PDouble
  | PDynamic
  | PFloat
  | PInt
  | PUint
  | PLong
  | PUlong
  | PObject
  | PShort
  | PString
  | PUshort
  | PVar
  | PVoid
  deriving (Eq, Show)

data AccessMod
  = Public
  | Private
  | Protected
  | Internal
  deriving (Eq, Show)

data TypeMod
  = Abstract
  | Async
  | Const
  | Event
  | Extern
  | New
  | Override
  | Partial
  | Readonly
  | Sealed
  | Static
  | Unsafe
  | Virtual
  | Volatile
  deriving (Eq, Show)

