module Analyser where

import qualified Data.Map.Strict as Map
import Control.Applicative ((<$>),(<*>))
import AST

data VarState = Undeclared | Uninitialised | Unsafe | Safe
    deriving (Show, Ord, Eq, Enum)

data VarScope = VarScope { mem :: Map.Map String (Typeid, VarState)
                         , par :: VarScope }
              | EmptyScope
    deriving Show

subscope x = VarScope Map.empty x

sclookup _ EmptyScope = Nothing
sclookup k (VarScope mem par) = case Map.lookup k mem of
                                     Just x -> Just x
                                     Nothing -> sclookup k par

updatestate Safe = Safe
updatestate x = succ x

scupdate k v (VarScope mem par) = case Map.updateLookupWithKey (\_ (tid,_) -> Just (tid,v)) k mem of
                                       (Just _, memu) -> VarScope memu par
                                       (Nothing, _) -> VarScope mem (scupdate k v par)

-- addfdecls :: VarScope -> [FuncDecl] -> VarScope
-- addfdecls sc [] = sc
-- addfdecls (VarScope sc par) (FuncDecl tid nid pdecl fsc : fs) = VarScope (addfdecls sc1 fs) par
--     where sc1 = Map.insert nid (tid, Safe) sc

addvdecls :: VarScope -> [VarDecl] -> VarScope
addvdecls sc [] = sc
addvdecls (VarScope sc par) (VarDecl tid nid ini : vs) = addvdecls sc1 vs
    where sc1 = VarScope (Map.insert nid (tid, state ini) sc) par
          state (Nothing) = Uninitialised
          state (Just _)  = Safe

class Typed a where
    analyse :: a -> Either String a
    analyse a = case analyses a (subscope EmptyScope) of Left x -> Left x; Right _ -> Right a
    analyses :: a -> VarScope -> Either String VarScope

instance Typed Program where
    analyses (Program _ psc) sc = analyses psc sc

instance Typed Scope where
     analyses (Scope vs cmds) sc = un sc <$> sc1
        where ssc = addvdecls (subscope sc) vs
              sc1 = par <$> foldl (\a x -> analyses x =<< a) (Right ssc) cmds
              un EmptyScope x = x
              un x EmptyScope = x
              un a b = VarScope (union (mem a) (mem b)) (un (par a) (par b))
              union = Map.unionWith (\(t1,s1) (t2,s2) -> if s1 == Uninitialised && s2 == Safe then (t2, Unsafe) else (t2,s2))

instance Typed Command where
    analyses (Attrib (Var nid _) ex) sc = case (==) <$> tid <*> typeof ex sc of
                                               Right True -> Right $ scupdate nid Safe sc
                                               Right False -> Left $ "Wrong type in assignment of " ++ nid
                                               Left x -> Left x
        where tid = case sclookup nid sc of
                         Nothing -> Left $ "Undeclared identifier: " ++ nid
                         Just (t,_) -> Right t

    analyses (If cond scope Nothing) sc = case (==Bool) <$> typeof cond sc of
                                                 Left x -> Left x
                                                 Right False -> Left "Non boolean expression in if statement"
                                                 Right True -> analyses scope sc

typeof :: Expr -> VarScope -> Either String Typeid
typeof (IntLit _) sc = Right Int
typeof (BoolLit _) sc = Right Bool
typeof (StringLit _) sc = Right String
typeof (Var v is) sc =
    case sclookup v sc of
        Nothing -> Left $ "Undeclared identifier: " ++ v
        Just (typeid, state) -> case state of
                                     Uninitialised -> Left $ "Uninitialised variable: " ++ v
                                     Unsafe -> Left $ "Variable might not have been initialised: " ++ v
                                     _ -> Right typeid
typeof (BinOp e1  "+" e2) sc = binOpt  "+" e1 e2 Int  Int sc
typeof (BinOp e1  "-" e2) sc = binOpt  "-" e1 e2 Int  Int sc
typeof (BinOp e1  "*" e2) sc = binOpt  "*" e1 e2 Int  Int sc
typeof (BinOp e1  "/" e2) sc = binOpt  "/" e1 e2 Int  Int sc
typeof (BinOp e1  ">" e2) sc = binOpt  ">" e1 e2 Int Bool sc
typeof (BinOp e1  "<" e2) sc = binOpt  "<" e1 e2 Int Bool sc
typeof (BinOp e1 ">=" e2) sc = binOpt ">=" e1 e2 Int Bool sc
typeof (BinOp e1 "<=" e2) sc = binOpt "<=" e1 e2 Int Bool sc
typeof (BinOp e1 "==" e2) sc =
    case (==) <$> t1 <*> t2 of
            Right False -> Left "Cannot compare different types"
            Right True -> Right Bool
            Left e -> Left e
    where t1 = typeof e1 sc
          t2 = typeof e2 sc

binOpt :: [Char] -> Expr -> Expr -> Typeid -> Typeid -> VarScope -> Either String Typeid
binOpt op e1 e2 t r mem =
    case (&&) <$> ((==t) <$> t1) <*> ((==) <$> t1 <*> t2) of
            Right False -> Left $ "Expecting type " ++ show t ++ " for operands of " ++ op
            Right True -> Right r
            Left e -> Left e
    where t1 = typeof e1 mem
          t2 = typeof e2 mem

