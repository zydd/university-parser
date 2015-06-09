module StackMachine where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec
import Data.Vector ((!?))

data Data = Int Int | String String | Bool Bool | Var String
    deriving (Show,Eq)

data Instruction = Push Data | Add | Sub | Mult | Div | Not | Load
                 | Attr | Lt | Gt | Eq | If | While | Deref | Brl Int | Jumpl Int
    deriving (Show,Eq)

type Stack = [Data]
type Program = Vec.Vector Instruction
type Memory = Map.Map String Data

exec :: Program -> (Stack,Memory,Int) -> (Stack,Memory,Int)
exec prog sm@(_,_,ip) = case prog !? ip of
                             Just ins -> exec prog (inc $ instr sm ins)
                             Nothing -> sm
    where inc (s,m,ip) = (s,m,ip+1)

instr :: (Stack,Memory,Int) -> Instruction -> (Stack,Memory,Int)
instr (st, mem, ip) (Push x) = (x : st, mem, ip)

instr (Int y : Int  x : st, mem, ip) Add  = (Int (x + y) : st, mem, ip)
instr (Int y : Int  x : st, mem, ip) Sub  = (Int (x - y) : st, mem, ip)
instr (Int y : Int  x : st, mem, ip) Mult = (Int (x * y) : st, mem, ip)
instr (Int y : Int  x : st, mem, ip) Div  = (Int (div x y) : st, mem, ip)
instr (Int y : Int  x : st, mem, ip) Lt   = (Bool (x < y) : st, mem, ip)
instr (Int y : Int  x : st, mem, ip) Gt   = (Bool (x > y) : st, mem, ip)
instr (    y :      x : st, mem, ip) Eq   = (Bool (x == y) : st, mem, ip)
instr (    y : Var  x : st, mem, ip) Attr = (st, Map.insert x y mem, ip)

instr (Bool x : st, mem, ip) Not = (Bool (not x) : st, mem, ip)

instr (st, mem, ip) (Jumpl n) = (st, mem, ip + n)

instr (Bool True : st, mem, ip) (Brl n) = (st, mem, ip + n)
instr (Bool False : st, mem, ip) (Brl n) = (st, mem, ip)

instr (Var x : st, mem, ip) Deref = (v : st, mem, ip)
    where v = case Map.lookup x mem of
                   Just x -> x
                   Nothing -> error $ "undefined: " ++ x

instr (x : _, _, _) i = error $  '(' : show x ++ ") " ++ show i

instr ([], _, _) i = error $ "empty stack: " ++ show i

