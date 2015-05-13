module StackMachine (exec) where

import qualified Data.Map.Strict as Map

data Data = Int Int | String String | Bool Bool | Var String
    deriving Show
data Instruction = Push Data | Add | Sub | Mult | Div | Attr

type Stack = [Data]
type Program = [Instruction]
type Memory = Map.Map String Data

(Int x) |+| (Int y) = Int (x + y)
(Int x) |-| (Int y) = Int (x - y)
(Int x) |*| (Int y) = Int (x * y)
(Int x) |/| (Int y) = Int (x `div` y)

deref :: Memory -> Data -> Data
deref mem (Var x) = case Map.lookup x mem of Just x -> x
deref _ x = x

exec :: (Stack,Memory) -> Program -> (Stack,Memory)
exec (st, mem) (Push x : ins) = exec (x : st, mem) ins

exec (y : x : st, mem) (Add  : ins) = exec (deref mem x |+| deref mem y : st, mem) ins
exec (y : x : st, mem) (Sub  : ins) = exec (deref mem x |-| deref mem y : st, mem) ins
exec (y : x : st, mem) (Mult : ins) = exec (deref mem x |*| deref mem y : st, mem) ins
exec (y : x : st, mem) (Div  : ins) = exec (deref mem x |/| deref mem y : st, mem) ins

exec (y : Var x : st, mem) (Attr : ins) = exec (st, Map.insert x y mem) ins

exec sm [] = sm

