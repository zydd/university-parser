module Compiler(compile) where

import qualified AST as A
import StackMachine

class Stackable a where
    compile :: a -> [Instruction]

opins "+" = Add
opins "-" = Sub
opins "*" = Mult
opins "/" = Div
opins "<" = Lt
opins ">" = Gt
opins "==" = Eq

instance Stackable A.Expr where
    compile (A.BinOp e1 op e2) = compile e1 ++ compile e2 ++ [opins op]
    compile (A.Not e) = compile e ++ [Not]
    compile (A.IntLit x) = [Push $ Int x]
    compile (A.BoolLit b) = [Push $ Bool b]
    compile (A.Var n []) = [Push $ Var n, Deref]

instance Stackable A.Program where
    compile (A.Program _ s) = compile s

instance Stackable A.Scope where
    compile (A.Scope _ cs) = concat $ map compile cs

instance Stackable A.Command where
    compile (A.Expr e) = compile e -- ++ [Pop]
    compile (A.Attrib (A.Var v []) e) = [Push $ Var v] ++ compile e ++ [Attr]
    compile (A.If e cs Nothing) = compile e ++ [Not, Jumpl $ length block] ++ block
        where block = compile cs
    compile (A.While e cs) = cond ++ block
        where cond = compile e ++ [Not, Brl $ length block]
              block = compile cs ++ [Jumpl $ -(length cond + 1)]
