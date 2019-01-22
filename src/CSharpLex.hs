module CSharpLex where

import Data.Char
import Control.Monad
import ParseLib.Abstract
import Prelude hiding ((<*), (<$), (*>))


data Token = POpen    | PClose      -- parentheses     ()
           | SOpen    | SClose      -- square brackets []
           | COpen    | CClose      -- curly braces    {}
           | Comma    | Semicolon
           | KeyIf    | KeyElse
           | KeyFor                 -- For loopy <3
           | KeyWhile | KeyReturn
           | KeyTry   | KeyCatch
           | KeyClass | KeyVoid
           | StdType   String       -- the 8 standard types
           | Operator  String       -- the 15 operators
           | UpperId   String       -- uppercase identifiers
           | LowerId   String       -- lowercase identifiers
           | ConstInt  Int
           | ConstBool Bool
           deriving (Eq, Show)

keyword :: String -> Parser Char String
keyword [] = succeed ""
keyword xs@(x:_) | isLetter x = do ys <- greedy (satisfy isAlphaNum)
                                   guard (xs == ys)
                                   return ys
                 | otherwise  = token xs


greedyChoice :: [Parser s a] -> Parser s a
greedyChoice = foldr (<<|>) empty


terminals :: [(Token, String)]
terminals =
    [ ( POpen     , "("      )
    , ( PClose    , ")"      )
    , ( SOpen     , "["      )
    , ( SClose    , "]"      )
    , ( COpen     , "{"      )
    , ( CClose    , "}"      )
    , ( Comma     , ","      )
    , ( Semicolon , ";"      )
    , ( KeyIf     , "if"     )
    , ( KeyElse   , "else"   )
    , ( KeyFor    , "for"    )
    , ( KeyWhile  , "while"  )
    , ( KeyReturn , "return" )
    , ( KeyTry    , "try"    )
    , ( KeyCatch  , "catch"  )
    , ( KeyClass  , "class"  )
    , ( KeyVoid   , "void"   )
    ]


lexWhiteSpace :: Parser Char String
lexWhiteSpace = greedy (satisfy isSpace)

lexComment :: Parser Char String
lexComment = pack (token "//") (many (satisfy (/='\n'))) (symbol '\n')

lexLowerId :: Parser Char Token
lexLowerId = (\x xs -> LowerId (x:xs)) <$> satisfy isLower <*> greedy (satisfy isAlphaNum)

lexUpperId :: Parser Char Token
lexUpperId = (\x xs -> UpperId (x:xs)) <$> satisfy isUpper <*> greedy (satisfy isAlphaNum)

lexConstBool :: Parser Char Token
lexConstBool = ConstBool True  <$ token "true" <|>
               ConstBool False <$ token "false"

lexConstChar :: Parser Char Token
lexConstChar = ConstInt . ord <$> pack (satisfy (=='\'')) anySymbol (satisfy (=='\'')) 

lexConstInt :: Parser Char Token
lexConstInt = ConstInt . read <$> greedy1 (satisfy isDigit)

lexEnum :: (String -> Token) -> [String] -> Parser Char Token
lexEnum f xs = f <$> choice (map keyword xs)

lexTerminal :: Parser Char Token
lexTerminal = choice [t <$ keyword s | (t,s) <- terminals]


stdTypes :: [String]
stdTypes = ["int", "long", "double", "float", "byte", "short", "bool", "char"]

operators :: [String]
operators = ["+", "-", "*", "/", "%", "&&", "||", "^", "<=", "<", ">=", ">", "==", "!=", "="]


lexToken :: Parser Char Token
lexToken = greedyChoice
             [ lexTerminal
             , lexEnum StdType stdTypes
             , lexEnum Operator operators
             , lexConstInt
             , lexConstBool
             , lexConstChar
             , lexLowerId
             , lexUpperId
             ]

lexicalScanner :: Parser Char [Token]
lexicalScanner = lexWhiteSpace *> greedy (lexToken <* lexWhiteSpace <* many (lexComment <* lexWhiteSpace)) <* eof


sStdType :: Parser Token Token
sStdType = satisfy isStdType
    where isStdType (StdType _) = True
          isStdType _           = False

sUpperId :: Parser Token Token
sUpperId = satisfy isUpperId
    where isUpperId (UpperId _) = True
          isUpperId _           = False

sLowerId :: Parser Token Token
sLowerId = satisfy isLowerId
    where isLowerId (LowerId _) = True
          isLowerId _           = False

sConst :: Parser Token Token
sConst  = satisfy isConst
    where isConst (ConstInt  _) = True
          isConst (ConstBool _) = True
          isConst _             = False

sOperatorAddis :: Parser Token Token
sOperatorAddis = satisfy isOperator
  where isOperator (Operator "+") = True
        isOperator (Operator "-") = True
        isOperator _              = False

sOperatorMultis :: Parser Token Token
sOperatorMultis = satisfy isOperator
  where isOperator (Operator "*") = True
        isOperator (Operator "/") = True
        isOperator (Operator "%") = True
        isOperator _              = False

sOperatorComparisonLessAndGreaterThan :: Parser Token Token
sOperatorComparisonLessAndGreaterThan = satisfy isOperator
  where isOperator (Operator "<=") = True
        isOperator (Operator "<") = True
        isOperator (Operator ">=") = True
        isOperator (Operator ">") = True
        isOperator _              = False

sOperatorLevelComparisonEqualAndNotEqual :: Parser Token Token
sOperatorLevelComparisonEqualAndNotEqual = satisfy isOperator
  where isOperator (Operator "!=") = True
        isOperator (Operator "==") = True
        isOperator _               = False

sOperatorBitwiseXOR :: Parser Token Token
sOperatorBitwiseXOR = satisfy isOperator
  where isOperator (Operator "^") = True
        isOperator _              = False

sOperatorBitwiseAnd :: Parser Token Token
sOperatorBitwiseAnd = satisfy isOperator
  where isOperator (Operator "&&") = True
        isOperator _               = False

sOperatorBitwiseOr :: Parser Token Token
sOperatorBitwiseOr = satisfy isOperator
  where isOperator (Operator "||") = True
        isOperator _               = False

sOperatorAssignment :: Parser Token Token
sOperatorAssignment = satisfy isOperator
  where isOperator (Operator "=") = True
        isOperator _              = False

--According to https://en.wikipedia.org/wiki/Order_of_operations

sSemi :: Parser Token Token
sSemi =  symbol Semicolon

