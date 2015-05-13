module Compiler where

import AST
import qualified StackMachine as S


import qualified Parser as P
import Text.Parsec.String

class Stackable a where
    compile :: a -> S.Program

instance Stackable Expr where
    compile (BinOp e1 "+" e2) = compile e1 ++ compile e2 ++ [S.Add]
    compile (BinOp e1 "-" e2) = compile e1 ++ compile e2 ++ [S.Sub]
    compile (BinOp e1 "*" e2) = compile e1 ++ compile e2 ++ [S.Mult]
    compile (BinOp e1 "/" e2) = compile e1 ++ compile e2 ++ [S.Div]
    compile (IntLit x) = [S.Push $ S.Int x]
    compile (Var n []) = [S.Push $ S.Var n]

instance Stackable Program where
    compile (Program _ s) = compile s

instance Stackable Scope where
    compile (Scope _ cs) = concat $ map compile cs

instance Stackable Command where
    compile (Expr e) = compile e -- ++ [Pop]
    compile (Attrib v e) = compile v ++ compile e ++ [S.Attr]

