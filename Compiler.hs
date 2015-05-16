module Compiler(compile) where

import AST
import qualified StackMachine as S

class Stackable a where
    compile :: a -> S.Program

instr "+" = S.Add
instr "-" = S.Sub
instr "*" = S.Mult
instr "/" = S.Div
instr "<" = S.Lt
instr ">" = S.Gt
instr "==" = S.Eq

instance Stackable Expr where
    compile (BinOp e1 op e2) = compile e1 ++ compile e2 ++ [instr op]
    compile (Not e) = compile e ++ [S.Not]
    compile (IntLit x) = [S.Push $ S.Int x]
    compile (BoolLit b) = [S.Push $ S.Bool b]
    compile (Var n []) = [S.Push $ S.Var n]

instance Stackable Program where
    compile (Program _ s) = compile s

instance Stackable Scope where
    compile (Scope _ cs) = concat $ map compile cs

instance Stackable Command where
    compile (Expr e) = compile e -- ++ [S.Pop]
    compile (Attrib v e) = compile v ++ compile e ++ [S.Attr]
    compile (If e cs Nothing) = [S.Push $ S.Block $ concat $ map compile cs] ++ compile e ++ [S.If]
    compile (While e cs) = [S.Push (S.Block $ concat (map compile cs) ++ cond)] ++ cond ++ [S.While]
        where cond = compile e

