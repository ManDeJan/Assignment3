module CSharpGram where

import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import CSharpLex
import Prelude hiding ((<*), (<$), (*>))


data Class = Class Token [Member]
    deriving Show

data Member = MemberD Decl
            | MemberM Type Token [Decl] Stat
            deriving Show

data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatReturn Expr
          | StatBlock  [Stat]
          deriving Show

data Expr = ExprConst  Token
          | ExprVar    Token
          | ExprOper   Token Expr Expr
          | ExprCall   Token [Expr]
          deriving Show

data Decl = Decl Type Token
    deriving Show

data Type = TypeVoid
          | TypePrim  Token
          | TypeObj   Token
          | TypeArray Type
          deriving (Eq,Show)

type Env = ([String], [String], [String]) -- Globals, Locals, Args

emptyEnv = ([],[],[])

parenthesised p = pack (symbol POpen) p (symbol PClose)
bracketed     p = pack (symbol SOpen) p (symbol SClose)
braced        p = pack (symbol COpen) p (symbol CClose)


pExpr' :: Parser Token Expr
pExpr' = chainl pExprMultis (ExprOper <$> sOperatorAddis) 

pExprMultis :: Parser Token Expr
pExprMultis = chainl pExprComparisonLessAndGreaterThan (ExprOper <$> sOperatorMultis)

pExprComparisonLessAndGreaterThan :: Parser Token Expr
pExprComparisonLessAndGreaterThan = chainl pExprComparisonEqualOrNotEqual (ExprOper <$> sOperatorComparisonLessAndGreaterThan)

pExprComparisonEqualOrNotEqual :: Parser Token Expr
pExprComparisonEqualOrNotEqual = chainl pExprBitwiseXOR (ExprOper <$> sOperatorLevelComparisonEqualAndNotEqual)

pExprBitwiseXOR :: Parser Token Expr
pExprBitwiseXOR = chainl pExprBitwiseAnd (ExprOper <$> sOperatorBitwiseXOR ) 

pExprBitwiseAnd :: Parser Token Expr
pExprBitwiseAnd = chainl pExprBitwiseOr (ExprOper <$> sOperatorBitwiseAnd) 

pExprBitwiseOr :: Parser Token Expr
pExprBitwiseOr = chainl pExprAssignment (ExprOper <$> sOperatorBitwiseOr) 

pExprAssignment :: Parser Token Expr
pExprAssignment = chainr pExprSimple (ExprOper <$> sOperatorAssignment)    

pExprSimple :: Parser Token Expr
pExprSimple =  ExprConst <$> sConst
           <|> ExprVar   <$> sLowerId
           <|> pExprCall
           <|> parenthesised pExpr'

pExprCall :: Parser Token Expr
pExprCall = ExprCall <$> sLowerId <*> parenthesised (option (listOf pExprSimple (symbol Comma)) [])

pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat =  StatExpr <$> pExpr' <*  sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised pExpr' <*> pStat <*> optionalElse
     <|> forLoop    <$ symbol KeyFor    <*  symbol POpen <*> pExpr' <* sSemi <*> pExpr' <* sSemi <*> pExpr' <* symbol PClose <*> pStat
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised pExpr' <*> pStat
     <|> StatReturn <$ symbol KeyReturn <*> pExpr'               <*  sSemi
     <|> pBlock
     where optionalElse = option ((\_ x -> x) <$> symbol KeyElse <*> pStat) (StatBlock [])
           forLoop init cond iter body = StatBlock [StatExpr init, StatWhile cond (StatBlock [body, StatExpr iter])] 

pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)

pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> (const TypeVoid <$> symbol KeyVoid)
        methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

pType0 :: Parser Token Type
pType0 =  TypePrim <$> sStdType
      <|> TypeObj  <$> sUpperId

pType :: Parser Token Type
pType = foldr (const TypeArray) <$> pType0 <*> many (bracketed (succeed ()))


pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = const <$> pDecl <*> sSemi

pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)
