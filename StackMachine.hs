module StackMachine where

import qualified Data.Map.Strict as Map

data Data = Int Int | String String | Bool Bool | Var String | Block [Instruction]
    deriving Show
data Instruction = Push Data | Add | Sub | Mult | Div | Attr | Not | Lt | Gt | Eq | If | While
    deriving Show

type Stack = [Data]
type Program = [Instruction]
type Memory = Map.Map String Data

instance Num Data where
    (Int x) + (Int y) = Int (x + y)
    (Int x) - (Int y) = Int (x - y)
    (Int x) * (Int y) = Int (x * y)
    abs (Int x) = Int (abs x)
    signum (Int x) = Int (signum x)
    fromInteger x = Int (fromInteger x)

(Int x) |/| (Int y) = Int (x `div` y)
(Int x) |>| (Int y) = Bool (x > y)
(Int x) |<| (Int y) = Bool (x < y)
(Int x) |==| (Int y) = Bool (x == y)

deref :: Memory -> Data -> Data
deref mem (Var x) = case Map.lookup x mem of Just x -> x
deref _ x = x

exec :: (Stack,Memory) -> Program -> (Stack,Memory)
exec (st, mem) (Push x : ins) = exec (x : st, mem) ins

exec (y : x : st, mem) (Add  : ins) = exec (deref mem x  +  deref mem y : st, mem) ins
exec (y : x : st, mem) (Sub  : ins) = exec (deref mem x  -  deref mem y : st, mem) ins
exec (y : x : st, mem) (Mult : ins) = exec (deref mem x  *  deref mem y : st, mem) ins
exec (y : x : st, mem) (Div  : ins) = exec (deref mem x |/| deref mem y : st, mem) ins

exec (y : x : st, mem) (Lt   : ins) = exec (deref mem x |<|  deref mem y : st, mem) ins
exec (y : x : st, mem) (Gt   : ins) = exec (deref mem x |>|  deref mem y : st, mem) ins
exec (y : x : st, mem) (Eq   : ins) = exec (deref mem x |==| deref mem y : st, mem) ins

exec (Bool x : st, mem) (Not : ins) = exec (Bool (not x) : st, mem) ins

exec (y : Var x : st, mem) (Attr : ins) = exec (st, Map.insert x y mem) ins

exec (Bool True  : Block blk : st, mem) (If : ins) = exec (exec (st,mem) blk) ins
exec (Bool False : Block blk : st, mem) (If : ins) = exec (st,mem) ins

exec (Bool True  : Block blk : st, mem) (While : ins) = exec (exec (Block blk : st,mem) blk) (While : ins)
exec (Bool False : Block blk : st, mem) (While : ins) = exec (st,mem) ins

exec sm [] = sm

