module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import Data.Map as M
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM


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
fClas c ms = [Bsr "main", HALT] ++ snd $ foldl (\(env, cod) mem -> (\(env', cod') -> (env', cod ++ cod')) (mem env)) (([],[],[]), []) ms

fMembDecl :: Decl -> (Env -> (Env, Code))
fMembDecl d = []

fMembMeth :: Type -> Token -> [Decl] -> (Env -> (Env, Code)) -> (Env -> (Env, Code))
fMembMeth t (LowerId x) ps s = [LABEL x] ++ s ++ [RET]

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
fExprCon (ConstInt  n) va     = [LDC n]
fExprCon (ConstBool b) va     = [LDC $ fromEnum b]

fExprVar :: Token -> (Env -> ValueOrAddress -> Code)
fExprVar (LowerId x) va = let loc = 37 in case va of
                                              Value    ->  [LDL  loc]
                                              Address  ->  [LDLA loc]

fExprOp :: Token -> (Env -> ValueOrAddress -> Code) -> (Env -> ValueOrAddress -> Code) -> (Env -> ValueOrAddress -> Code)
fExprOp (Operator "=") e1 e2 env va = e2 Value ++ [LDS 0] ++ e1 Address ++ [STA 0]
fExprOp (Operator op)  e1 e2 env va = e1 Value ++ e2 Value ++ [opCodes ! op]


opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]

