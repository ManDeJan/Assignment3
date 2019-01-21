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
          deriving Show

data Decl = Decl Type Token
    deriving Show

data Type = TypeVoid
          | TypePrim  Token
          | TypeObj   Token
          | TypeArray Type
          deriving (Eq,Show)


parenthesised p = pack (symbol POpen) p (symbol PClose)
bracketed     p = pack (symbol SOpen) p (symbol SClose)
braced        p = pack (symbol COpen) p (symbol CClose)


pExpr' :: Parser Token Expr
pExpr' = chainl pExpr1 (ExprOper <$> sOperatorLevel1) 

pExpr1 :: Parser Token Expr
pExpr1 = chainl pExpr2 (ExprOper <$> sOperatorLevel2)

pExpr2 :: Parser Token Expr
pExpr2 = chainl pExpr3 (ExprOper <$> sOperatorLevel3)

pExpr3 :: Parser Token Expr
pExpr3 = chainl pExpr4 (ExprOper <$> sOperatorLevel4) 

pExpr4 :: Parser Token Expr
pExpr4 = chainl pExpr5 (ExprOper <$> sOperatorLevel5)

pExpr5 :: Parser Token Expr
pExpr5 = chainl pExpr6 (ExprOper <$> sOperatorLevel6)

pExpr6 :: Parser Token Expr
pExpr6 = chainl pExpr7 (ExprOper <$> sOperatorLevel7) 

pExpr7 :: Parser Token Expr
pExpr7 = chainl pExpr8 (ExprOper <$> sOperatorLevel8) 

pExpr8 :: Parser Token Expr
pExpr8 = chainl pExpr9 (ExprOper <$> sOperatorLevel9) 

pExpr9 :: Parser Token Expr
pExpr9 = chainl pExpr10 (ExprOper <$> sOperatorLevel10)

pExpr10 :: Parser Token Expr
pExpr10 = chainl pExpr11 (ExprOper <$> sOperatorLevel11) 

pExpr11 :: Parser Token Expr
pExpr11 = chainl pExpr12 (ExprOper <$> sOperatorLevel2) 

pExpr12 :: Parser Token Expr
pExpr12 = chainl pExprSimple (ExprOper <$> sOperatorLevel13) 
     

pExprSimple :: Parser Token Expr
pExprSimple =  ExprConst <$> sConst
           <|> ExprVar   <$> sLowerId
           <|> parenthesised pExpr'

pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat =  StatExpr <$> pExpr' <*  sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised pExpr' <*> pStat <*> optionalElse
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised pExpr' <*> pStat
     <|> StatReturn <$ symbol KeyReturn <*> pExpr'              <*  sSemi
     <|> pBlock
     where optionalElse = option ((\_ x -> x) <$> symbol KeyElse <*> pStat) (StatBlock [])


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

