{-# LANGUAGE TupleSections #-}

module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import qualified Data.Map as M
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
    , (fExprCon, fExprVar, fExprOp)
    )

fClas :: Token -> [Env -> (Env, Code)] -> Code
fClas c ms = [Bsr "main", HALT] ++ snd $ foldl (\(env, cod) mem -> -- What is love
                                                   (\(env', cod') ->    -- baby don't hurt me
                                                       (env', cod ++ cod')) (mem env)) -- haskell is love
                                               (emptyEnv, []) -- haskell is life
                                               ms

fMembDecl :: Decl -> (Env -> (Env, Code))
fMembDecl (Decl typ (LowerId id)) = (,[]) . addGlob id

fMembMeth :: Type -> Token -> [Decl] -> (Env -> (Env, Code)) -> (Env -> (Env, Code))
fMembMeth t (LowerId id) args s env = (env, [LINK localVarCount, LABEL id] ++ cod ++ [UNLINK, RET])
    where env' = foldl (\envAcc (Decl _ (LowerId id')) -> addArg id' envAcc) env args
          ((_,localVars,_), cod) = s env'
          localVarCount = length localVars
          

    --int square(int x,int y){
--     int z;
--     int a;
-- }

fStatDecl :: Decl -> (Env -> (Env, Code))
fStatDecl d = []

fStatExpr :: (Env -> ValueOrAddress -> Code) -> (Env -> (Env, Code))
fStatExpr e = e Value ++ [pop]

fStatIf :: (Env -> ValueOrAddress -> Code) -> (Env -> (Env, Code)) -> (Env -> (Env, Code)) -> (Env -> (Env, Code))
fStatIf e s1 s2 = c ++ [BRF (n1 + 2)] ++ s1 ++ [BRA n2] ++ s2
    where
        c        = e Value
        (n1, n2) = (codeSize s1, codeSize s2)

fStatWhile :: (Env -> ValueOrAddress -> Code) -> (Env -> (Env, Code)) -> (Env -> (Env, Code))
fStatWhile e s1 = [BRA n] ++ s1 ++ c ++ [BRT (-(n + k + 2))]
    where
        c = e Value
        (n, k) = (codeSize s1, codeSize c)

fStatReturn :: (Env -> ValueOrAddress -> Code) -> (Env -> (Env, Code))
fStatReturn e = e Value ++ [pop] ++ [RET]

fStatBlock :: [Env -> (Env, Code)] -> (Env -> (Env, Code))
fStatBlock = concat

fExprCon :: Token -> (Env -> ValueOrAddress -> Code)
fExprCon (ConstInt  n) env va     = [LDC n]
fExprCon (ConstBool b) env va     = [LDC $ fromEnum b]

fExprVar :: Token -> (Env -> ValueOrAddress -> Code)
fExprVar (LowerId x) env va = let loc = 37 in case va of
                                              Value    ->  [LDL  loc]
                                              Address  ->  [LDLA loc]

fExprOp :: Token -> (Env -> ValueOrAddress -> Code) -> (Env -> ValueOrAddress -> Code) -> (Env -> ValueOrAddress -> Code)
fExprOp (Operator "=") e1 e2 env va = e2' ++ [LDS 0] ++ e1' ++ [STA 0]
  where e1'     = e1 env Address
        e2'     = e2 env Value
fExprOp (Operator op) e1 e2 env va
  | isAnd     = e1' ++ [BRF (n + k + 4)] ++ e2' ++ [BRF 4, LDC 1, BRA 2, LDC 0]
  | isOr      = e1' ++ [BRT (n + k + 4)] ++ e2' ++ [BRT 4, LDC 0, BRA 2, LDC 1]
  | otherwise = e1' ++ e2' ++ [opCodes M.! op]
    where isAnd = op == "&&"
          isOr  = op == "||"
          e1'   = e1 env Value
          e2'   = e2 env Value
          (n, k)= (codeSize e1', codeSize e2')

opCodes :: M.Map String Instr
opCodes = M.fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                     , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                     , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                     ]

