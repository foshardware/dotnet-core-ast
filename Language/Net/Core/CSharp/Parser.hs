{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Language.Net.Core.CSharp.Parser where

import Control.Applicative hiding (Const)
import Control.Monad
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec (ParseError, token, parse)
import Text.Parsec.String (GenParser)
import Text.Parsec.Combinator (sepBy, sepBy1, sepEndBy, between)
import Text.Parsec.Pos
import Prelude hiding (null)

import Language.Net.Core.CSharp.Lexer
import Language.Net.Core.CSharp.Syntax


type Parser = GenParser (Lexer Token) ()

parseAST :: Text -> Either ParseError AST
parseAST = parse ast [] . lexer []

ast :: Parser AST
ast = AST <$> some file

file :: Parser File
file = File <$> many importDec <*> many namespaceDec

name :: Parser Name
name = ident `sepBy` dot

importDec :: Parser Import
importDec = using >> Import <$> name <*> optional (assign_ *> name) <* semi

namespaceDec :: Parser Namespace
namespaceDec
  = namespace >> Namespace
  <$> name
  <*> between lbrace rbrace (many typeDec)

typeDec :: Parser TypeDec
typeDec
  = TypeDec
  <$> attr
  <*> typeDef

typeDef :: Parser TypeDef
typeDef
  =   EnumDef  <$> enumDec
  <|> ClassDef <$> classDec

attr :: Parser Attr
attr = Attr <$> many typeAttr <*> optional accessMod <*> many typeMod

enumDec :: Parser EnumDec
enumDec
  = enum_ >> EnumDec
  <$> type_
  <*> between lbrace rbrace (ident `sepEndBy` comma)

classDec :: Parser ClassDec
classDec
  = ClassDec
  <$> module_
  <*> type_
  <*> (colon *> type_ `sepBy1` comma <|> pure [])
  <*> (where_ *> typeInheritance `sepBy` comma <|> pure [])
  <*> between lbrace rbrace (many memberDec)

typeInheritance :: Parser TypeInheritance
typeInheritance = (,) <$> type_ <*> (colon *> type_)

accessMod :: Parser AccessMod
accessMod
  =   Public    <$ p Tok_Public
  <|> Private   <$ p Tok_Private
  <|> Protected <$ p Tok_Protected
  <|> Internal  <$ p Tok_Internal

typeMod :: Parser TypeMod
typeMod
  =   Abstract <$ p Tok_Abstract
  <|> Async    <$ p Tok_Async
  <|> Const    <$ p Tok_Const
  <|> Event    <$ p Tok_Event
  <|> Extern   <$ p Tok_Extern
  <|> New      <$ p Tok_New
  <|> Override <$ p Tok_Override
  <|> Partial  <$ p Tok_Partial
  <|> Readonly <$ p Tok_Readonly
  <|> Sealed   <$ p Tok_Sealed
  <|> Static   <$ p Tok_Static
  <|> Unsafe   <$ p Tok_Unsafe
  <|> Virtual  <$ p Tok_Virtual
  <|> Volatile <$ p Tok_Volatile

module_ :: Parser Module
module_
  =   ModClass     <$ p Tok_Class
  <|> ModInterface <$ p Tok_Interface
  <|> ModDelegate  <$ p Tok_Delegate

memberDec :: Parser MemberDec
memberDec
  = MemberDec
  <$> attr
  <*> memberDef

memberDef :: Parser MemberDef
memberDef
  =   MemberClass <$> classDec 
  <|> MemberEnum  <$> enumDec 
  <|> Member <$> typeIdent <*> methodDec `eitherOr` fieldDec

typeIdent :: Parser TypeIdent
typeIdent
  = TypeIdent
  <$> type_
  <*> optional ident

methodDec :: Parser MethodDec
methodDec
  = MethodDec
  <$> between lparen rparen (argument `sepBy` comma)
  <*> (colon  *> base *> between lparen rparen (expr `sepBy` comma) <|> pure [])
  <*> (where_ *> typeInheritance `sepBy` comma <|> pure [])
  <*> (between lbrace rbrace (many stmt) <|> [] <$ semi)

fieldDec :: Parser FieldDec 
fieldDec
  = FieldDec
  <$> optional (between lbrace rbrace fieldTransform)
  <*> optional (assign_ <|> lambda >> expr)
  <*  optional semi

fieldTransform :: Parser FieldTransform
fieldTransform
  = FieldTransform 
  <$> optional (get_ *> trans)
  <*> optional (set_ *> trans)
  where trans = eitherOr (assign_ *> expr) (block <|> pure [])

typeAttr :: Parser TypeAttr
typeAttr = between lbrack rbrack (next `sepBy1` comma)

argument :: Parser Arg
argument
  = Arg
  <$> many typeAttr
  <*> type_
  <*> optional argMod
  <*> optional ident
  <*> optional (assign_ *> expr)

type_ :: Parser Type
type_
  =   Identity <$> module_
  <|> primitiveType
  <|> higherOrderType
  <|> arrayType
  <|> customType
  <|> nullableType

variantType :: Parser (Variant Type)
variantType
  =   Covariant     <$> (in_  >> type_)
  <|> Contravariant <$> (out_ >> type_)
  <|> Invariant     <$> type_ 

nullableType :: Parser Type
nullableType = Nullable <$> maybeNull

higherOrderType :: Parser Type
higherOrderType
  = HigherOrder
  <$> higherOrder
  <*> (variantType `sepBy` comma)
  <*> (gt *> optional argMod)

customType :: Parser Type
customType = Type <$> capitalizedIdent <*> optional argMod

argMod :: Parser ArgMod
argMod = Array <$> between lbrack rbrack (optional expr)

primitiveType :: Parser Type
primitiveType
  = PrimitiveType <$> prim <*> optional argMod
  where 
  prim = PBool    <$ p Tok_Bool
     <|> PByte    <$ p Tok_Byte
     <|> PSbyte   <$ p Tok_Sbyte
     <|> PChar    <$ p Tok_Char
     <|> PDecimal <$ p Tok_Decimal
     <|> PDouble  <$ p Tok_Double
     <|> PDynamic <$ p Tok_Dynamic
     <|> PFloat   <$ p Tok_Float
     <|> PInt     <$ p Tok_Int
     <|> PUint    <$ p Tok_UInt
     <|> PLong    <$ p Tok_Long
     <|> PUlong   <$ p Tok_ULong
     <|> PObject  <$ p Tok_Object
     <|> PShort   <$ p Tok_Short
     <|> PString  <$ p Tok_String
     <|> PUshort  <$ p Tok_UShort
     <|> PVar     <$ p Tok_Var
     <|> PVoid    <$ p Tok_Void

stmt :: Parser Stmt
stmt
  =   EmptyStmt <$  semi
  <|> Exception <$> (throw_ *> next <* semi)
  <|> Return    <$> (return_ *> optional next <* semi)
  <|> Break     <$  break_    <* semi
  <|> Continue  <$  continue_ <* semi
  <|> ifStatement
  <|> caseStatement
  <|> usingStatement
  <|> tryCatch
  <|> forLoop
  <|> forEachLoop
  <|> Stmt <$> bind <* semi
  <|> Stmt <$> next <* semi
  where bind = Bind <$> type_ <*> (Op <$> operator <|> next)

block :: Parser [Stmt]
block
  =   join <$> between lbrace rbrace (many block)
  <|> pure <$> stmt

ifStatement :: Parser Stmt
ifStatement
  = if_ >> IfStmt
  <$> between lparen rparen next 
  <*> block
  <*> (else_ *> block <|> pure []) 

caseStatement :: Parser Stmt
caseStatement
  = switch_ >> CaseStmt
  <$> between lparen rparen next
  <*> (lbrace *> many caseDecl)
  <*> (optional defaultDecl <* rbrace)
  where
  caseDecl = case_ >> (,) <$> next <*> (colon *> many stmt)
  defaultDecl = default_ >> colon *> many stmt

usingStatement :: Parser Stmt
usingStatement
  = using >> UsingStmt
  <$> between lparen rparen (Bind <$> type_ <*> next)
  <*> block

tryCatch :: Parser Stmt
tryCatch
  = try_ >> Catch
  <$> block
  <*> many (catch_ >> (,) <$> between lparen rparen argument <*> block)

forLoop :: Parser Stmt
forLoop
  = for >> ForLoop
  <$> (lparen *> optional (Bind <$> type_ <*> next) <* semi)
  <*> (optional next <* semi)
  <*> (optional next <* rparen) 
  <*> block

forEachLoop :: Parser Stmt
forEachLoop
  = foreach >> ForEachLoop
  <$> (lparen *> argument)
  <*> (in_ *> next <* rparen)
  <*> block

next, expr :: Parser Expr
next = Ex <$> expr <*> optional (Op <$> operator)
expr
  =   Ex <$> between lparen rparen next <*> optional expr
  <|> MaybeNull    <$> maybeNull
  <|> LitString    <$> string
  <|> Null         <$  null
  <|> LitVerbatim  <$> verbatim
  <|> LitInt       <$> int
  <|> LitReal      <$> real 
  <|> LitChar      <$> char 
  <|> LitBool      <$> bool 
  <|> TypeOf       <$> (typeof >> between lparen rparen type_)
  <|> Glue         <$> (new >> Op <$> recordAccess <|> next)
  <|> This         <$  this
  <|> Base         <$  base
  <|> Int          <$  int_
  <|> Double       <$  double_
  <|> String       <$  string_
  <|> Op . Await   <$> (await *> next)
  <|> Op . Out     <$> (out_  *> next)
  <|> Op           <$> indecrement
  <|> Ty           <$> type_
  <|> Point        <$> ident

operator :: Parser Operator
operator
  =   Await        <$> (await *> next)
  <|> Not          <$> (not_ *> next)
  <|> In           <$> (in_  *> next)
  <|> Out          <$> (out_ *> next)
  <|> Plus   <$> (plus    *> next)
  <|> Minus  <$> (minus   *> next)
  <|> Mult   <$> (star    *> next)
  <|> Div    <$> (slash   *> next)
  <|> Modulo <$> (percent *> next)
  <|> Eq             <$> (eq    *> next)
  <|> NotEq          <$> (notEq *> next)
  <|> LessOrEqual    <$> (ltEq  *> next)
  <|> Less           <$> (lt    *> next)
  <|> Greater        <$> (gt    *> next)
  <|> GreaterOrEqual <$> (gtEq  *> next)
  <|> Or             <$> (or_   *> next)
  <|> And            <$> (and_  *> next)
  <|> If     <$> (question *> next) <*> (colon *> next)
  <|> Assign <$> (assign_ *> next)
  <|> Cast   <$> (as_ *> type_)
  <|> Access       <$> (dot *> next)
  <|> NullAccess   <$> (maybedot *> next)
  <|> ArrayAccess  <$> between lbrack rbrack next <*> optional operator
  <|> NullArrayAccess <$> between maybebrack rbrack next <*> optional operator
  <|> recordAccess
  <|> MethodCall   <$> between lparen rparen (next `sepBy` comma) <*> optional operator
  <|> lambdaExpr

lambdaExpr :: Parser Operator
lambdaExpr
  = lambda >> Lambda
  <$> between lbrace rbrace (many stmt) `eitherOr` next

recordAccess :: Parser Operator
recordAccess = RecordAccess <$> between lbrace rbrace (next `sepEndBy` comma)

-----
--
-- think `optional` but for Either
eitherOr :: Parser a -> Parser b -> Parser (Either a b)
eitherOr a b = Left <$> a <|> Right <$> b

maybeToken :: (Token -> Maybe a) -> Parser a
maybeToken test = token showT posT testT
  where
  showT (L _ t) = show t
  posT  (L x _) = pos2sourcePos x
  testT (L _ t) = test t
  pos2sourcePos (l, c) = newPos "" l c

ident :: Parser Text
ident = maybeToken q
  where q (Tok_Ident t) = Just t
        q _ = Nothing

capitalizedIdent :: Parser Text
capitalizedIdent = maybeToken q
  where q (Tok_Ident t) | T.head t == toUpper (T.head t) && isLetter (T.head t)= Just t
        q _ = Nothing

higherOrder :: Parser Text
higherOrder = maybeToken q
  where q (Tok_HigherOrder t) = Just t
        q _ = Nothing

maybeNull :: Parser Text
maybeNull = maybeToken q
  where q (Tok_MaybeNull t) = Just t
        q _ = Nothing

arrayType :: Parser Type
arrayType =  Type <$> maybeToken q <*> optional argMod
  where q (Tok_ArrayType t) = Just t
        q _ = Nothing

indecrement :: Parser Operator
indecrement = maybeToken q
  where q (Tok_PostIncrement t) = Just $ PostIncrement t
        q (Tok_PostDecrement t) = Just $ PostDecrement t
        q (Tok_PreIncrement t) = Just $ PreIncrement t
        q (Tok_PreDecrement t) = Just $ PreDecrement t
        q _ = Nothing

string :: Parser Text
string = maybeToken q
  where q (Tok_StringLit t) = Just t
        q _ = Nothing

verbatim :: Parser Text
verbatim = maybeToken q
  where q (Tok_VerbatimLit t) = Just t
        q _ = Nothing

char :: Parser Char
char = maybeToken q
  where q (Tok_CharLit c) = Just (T.head c)
        q _ = Nothing

bool :: Parser Bool
bool = maybeToken q
  where q Tok_True  = Just True
        q Tok_False = Just False
        q _ = Nothing

int :: Parser Int
int = maybeToken q
  where q (Tok_IntLit i) = Just (read $ T.unpack i)
        q _ = Nothing

real :: Parser Text 
real = maybeToken q
  where q (Tok_RealLit t) = Just t 
        q _ = Nothing

anyTok :: Parser Token
anyTok = maybeToken pure

noneOf :: [Token] -> Parser ()
noneOf es = maybeToken $ \r -> if elem r es then Nothing else Just ()

-------------------------------
--
p :: Token -> Parser ()
p t = maybeToken $ \r -> if r == t then Just () else Nothing
and_ = p Tok_And
as_ = p Tok_As
assign_ = p Tok_Assign
await = p Tok_Await
base = p Tok_Base
break_ = p Tok_Break
case_ = p Tok_Case
catch_ = p Tok_Catch
colon = p Tok_Colon
comma = p Tok_Comma
continue_ = p Tok_Continue
decrement = p Tok_Decrement
default_ = p Tok_Default
dot = p Tok_Dot
double_ = p Tok_Double
else_ = p Tok_Else
enum_ = p Tok_Enum
eq = p Tok_Eq
for = p Tok_For
foreach = p Tok_Foreach
get_ = p Tok_Get
gt = p Tok_Gt
gtEq = p Tok_GtEq
if_ = p Tok_If
in_ = p Tok_In
increment = p Tok_Increment
int_ = p Tok_Int
namespace = p Tok_Namespace
null = p Tok_Null
semi = p Tok_Semi
lparen = p Tok_LParen
lbrack = p Tok_LBracket
lbrace = p Tok_LBrace
lambda = p Tok_Lambda
lt = p Tok_Lt
ltEq = p Tok_LtEq
maybedot = p Tok_MaybeDot
maybebrack = p Tok_MaybeBrack
minus = p Tok_Minus
new = p Tok_New
out_ = p Tok_Out
percent = p Tok_Percent
not_ = p Tok_Not
plus = p Tok_Plus
question = p Tok_Question
return_ = p Tok_Return
rparen = p Tok_RParen
rbrack = p Tok_RBracket
rbrace = p Tok_RBrace
slash = p Tok_Slash
star = p Tok_Star
string_ = p Tok_String
typeof = p Tok_Typeof
notEq = p Tok_NotEq
or_ = p Tok_Or
set_ = p Tok_Set
switch_ = p Tok_Switch
try_ = p Tok_Try
this = p Tok_This
throw_ = p Tok_Throw
using = p Tok_Using
where_ = p Tok_Where

