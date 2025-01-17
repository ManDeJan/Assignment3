{-# LANGUAGE TupleSections #-}
{-
  Authors:
    Jan Halsema,
    Quinten Stekelenburg  
-}
module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import qualified Data.Map as M
import Data.List
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM

addGlob, addLoc, addArg :: String -> Env -> Env
addGlob id (x,y,z) = (x ++ [id], y, z) 
addLoc  id (x,y,z) = (x, y ++ [id], z) 
addArg  id (x,y,z) = (x, y, z ++ [id]) 

data ValueOrAddress = Value | Address
    deriving Show

codeAlgebra :: CSharpAlgebra Code (Env -> (Env, Code)) (Env -> (Env, Code)) (Env -> ValueOrAddress -> Code)
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatBlock)
    , (fExprCon, fExprVar, fExprOp, fExprCall)
    )

{-
  Task 10
  The enviroment is created and then passed on to the next iteration, 
  so the environment grows after each iteration (if a new variable is declared that is). 
  Which allows local variables to be used.
-}
fClas :: Token -> [Env -> (Env, Code)] -> Code
fClas c ms = [Bsr "main", HALT] ++ snd (foldl (\(env, cod) mem ->
                                                  (\(env', cod') ->
                                                      (env', cod ++ cod')) (mem env))
                                              (emptyEnv, []) 
                                              ms)

fMembDecl :: Decl -> (Env -> (Env, Code))
fMembDecl (Decl typ (LowerId id)) = (,[]) . addGlob id

fMembMeth :: Type -> Token -> [Decl] -> (Env -> (Env, Code)) -> (Env -> (Env, Code))
fMembMeth t (LowerId id) args s env = (env, [LABEL id, LINK localVarCount] ++ cod ++ [UNLINK, RET])
    where env' = foldl (\envAcc (Decl _ (LowerId id')) -> addArg id' envAcc) env args
          ((_,locals,_), cod) = s env'
          localVarCount = length locals
          
fStatDecl :: Decl -> (Env -> (Env, Code))
fStatDecl (Decl typ (LowerId id)) = (,[]) . addLoc id

fStatExpr :: (Env -> ValueOrAddress -> Code) -> (Env -> (Env, Code))
fStatExpr e env = (env, e env Value ++ [pop])

fStatIf :: (Env -> ValueOrAddress -> Code) -> (Env -> (Env, Code)) -> (Env -> (Env, Code)) -> (Env -> (Env, Code))
fStatIf e s1 s2 env = (s2env, c ++ [BRF (n1 + 2)] ++ s1cod ++ [BRA n2] ++ s2cod)
    where
        c        = e env Value
        (n1, n2) = (codeSize s1cod, codeSize s2cod)
        (s1env,s1cod) = s1 env
        (s2env,s2cod) = s2 s1env

fStatWhile :: (Env -> ValueOrAddress -> Code) -> (Env -> (Env, Code)) -> (Env -> (Env, Code))
fStatWhile e s1 env = (s1env, [BRA n] ++ s1cod ++ c ++ [BRT (-(n + k + 2))])
    where
        c = e env Value
        (s1env, s1cod) = s1 env
        (n, k) = (codeSize s1cod, codeSize c)

fStatReturn :: (Env -> ValueOrAddress -> Code) -> (Env -> (Env, Code))
fStatReturn e env = (env, e env Value ++ [STR R3] ++ [UNLINK] ++ [RET])

{-
  Task 10
  The enviroment is created and then passed on to the next iteration, 
  so the environment grows after each iteration (if a new variable is declared that is). 
  Which allows local variables to be used.
-}
fStatBlock :: [Env -> (Env, Code)] -> (Env -> (Env, Code))
fStatBlock ms env = foldl (\(env, cod) mem ->
                        (\(env', cod') -> 
                            (env', cod ++ cod')) (mem env)) 
                    (env, [])
                    ms

{-
  Task 1
  Turns a boolean into a number and puts it on the stack.
  Same goes for an int, which could be a character
-}
fExprCon :: Token -> (Env -> ValueOrAddress -> Code)
fExprCon (ConstInt  n) _ _ = [LDC n]
fExprCon (ConstBool b) _ _ = [LDC $ fromEnum b]

{-
  Task 10
  Loads a variable, based on where it is declared.
-}
fExprVar :: Token -> (Env -> ValueOrAddress -> Code)
fExprVar (LowerId id) env@(_,locals,args) va = 
    let offset = findVarOffset id env in case va of
        Value    -> if local then [LDL  offset] else [LDC almostUnreasonablyLargeMagicNumber, LDA  offset]
        Address  -> if local then [LDLA offset] else [LDC almostUnreasonablyLargeMagicNumber, LDAA offset]
    where almostUnreasonablyLargeMagicNumber = 1000
          local = id `elem` (args ++ locals) 

--Bonus task, whenever a variable is not found anywhere, it must've not been declared so an error is thrown.
findVarOffset :: String -> Env -> Int
findVarOffset var (globs,locals,args) 
  | inArg     = getOffset (elemIndex var args) * (-2) - 1
  | inLocal   = getOffset (elemIndex var locals) + 1
  | inGlobal  = getOffset (elemIndex var globs)
  | otherwise = error "Variable not declared"
    where inArg    = var `elem` args
          inLocal  = var `elem` locals
          inGlobal = var `elem` globs

getOffset :: Maybe Int -> Int
getOffset (Just b) = b
getOffset Nothing  = error "Variable not declared"

{-
  Task 7
  Lazy evaluation for logical operators is done here. For an:
  Or : If the first argument is True, then the rest of the evaluation is skipped, and a 1 (true) is loaded onto the stack.
       If the first argument is False, the second argument is checked.
          If the second argument is False, a 0 is loaded and the rest of the code is skipped.
  And: If the first argument is False, then the rest of the evaluation is skipped, and a 0 (false) is loaded onto the stack
       If the the first argument is True, the second argument is checked
          if the second argument is true, a 1 is loaded, and the rest of the code is skipped. 
-}
fExprOp :: Token -> (Env -> ValueOrAddress -> Code) -> (Env -> ValueOrAddress -> Code) -> (Env -> ValueOrAddress -> Code)
fExprOp (Operator "=") e1 e2 env va = e2' ++ [LDS 0] ++ e1' ++ [STA 0]
    where e1'    = e1 env Address
          e2'    = e2 env Value
fExprOp (Operator op) e1 e2 env va
  | isAnd     = e1' ++ [BRF (n + k + 4)] ++ e2' ++ [BRF 4, LDC 1, BRA 2, LDC 0]
  | isOr      = e1' ++ [BRT (n + k + 4)] ++ e2' ++ [BRT 4, LDC 0, BRA 2, LDC 1]
  | otherwise = e1' ++ e2' ++ [opCodes M.! op]
    where isAnd  = op == "&&"
          isOr   = op == "||"
          e1'    = e1 env Value
          e2'    = e2 env Value
          (n, k) = (codeSize e1', codeSize e2')

{-
  Task 8 
  If a function call is to print, the TRAP 0 is repeated for the amount of arguments.

  Task 6
  Function calls are translated to SSM Code here, the arguments are handled in a reverse order, 
  this has to do with the way we store our arguments.

  Task 9
  The result of the functions are stored in the register, by using [LDR R3].
-}
fExprCall :: Token -> [Env -> ValueOrAddress -> Code] -> (Env -> ValueOrAddress -> Code)
fExprCall (LowerId "print") list env va = concatMap (\p -> p env Value) list ++ replicate (length list) (TRAP 0) ++ [LDR R3]
fExprCall (LowerId id) list env va      = concatMap (\p -> p env Value) (reverse list) ++ [Bsr id] ++ [AJS (-(length list))] ++ [LDR R3]

opCodes :: M.Map String Instr
opCodes = M.fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                     , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                     , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                     ]

