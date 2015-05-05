module Viz where

import AST

class Viz a where
    viz :: a -> String
    viz a = let (_,g) = vizs 0 a in "digraph {\n" ++ g ++ "}\n"
    vizs :: Int -> a -> (Int,String)

vizl i ([]) p = (i+1, show i ++ " [label=\""++ p ++"\"]\n")
vizl i xs p = vizls i xs p []

vizls i (x:[]) p c = (k, show i ++ " [label=\""++ p ++"\"]\n"
                      ++ show i ++ " -> " ++ show(i+1) ++ "\n"
                      ++ "{rank=same; " ++ foldr1 (++) (map ((++";") . show) (i:c)) ++ "}\n"
                      ++ ex)
    where (k,ex) = vizs (i+1) x

vizls i (x:xs) p c = (l, show i ++ " [label=\""++ p ++"\"]\n"
                      ++ show i ++ " -> " ++ show(i+1) ++ "\n"
                      ++ show i ++ " -> " ++ show(k) ++ "\n"
                      ++ ex ++ ex2)
    where (k,ex) = vizs (i+1) x
          (l,ex2) = vizls k xs p (i:c)

instance Viz Expr where
    vizs i (IntLit t) = (i+1, show i ++ " [label=\"Lit " ++ show t ++ "\"]\n")
    vizs i (BoolLit t) = (i+1,show i ++ " [label=\"Lit " ++ show t ++ "\"]\n")
    vizs i (StringLit t) = (i+1,show i ++ " [label=\"Lit \\\"" ++ t ++ "\\\"\"]\n")
    vizs i (Not t) = (j, show i ++ " [label=\"!\"]\n"
                      ++ show i ++ " -> " ++ show (i+1) ++ "\n"
                      ++ rhs)
        where (j,rhs) = vizs (i+1) t
    vizs i (BinOp t o u) = (k, show i ++ " [label=" ++ show o ++ "]\n"
                            ++ show i ++ " -> " ++ show (i+1) ++ "\n"
                            ++ show i ++ " -> " ++ show j ++ "\n"
                            ++ lhs ++ rhs)
        where (j,lhs) = vizs (i+1) t
              (k,rhs) = vizs j u
    vizs i (Var t []) = (i+1, show i ++ " [label=\"Var " ++ t ++ "\"]\n")
    vizs i (Var t u) = (j, show i ++ " [label=\"Var " ++ t ++ "\"]\n"
                        ++ show i ++ " -> " ++ show (i+1) ++ "\n"
                        ++ idx)
        where (j,idx) = vizl (i+1) u "[]"
    vizs i (Func t []) = (i+1, show i ++ " [label=\"Func " ++ t ++ "\"]\n")
    vizs i (Func t u) = (j, show i ++ " [label=\"Func " ++ t ++ "\"]\n"
                         ++ show i ++ " -> " ++ show (i+1) ++ "\n"
                         ++ idx)
        where (j,idx) = vizl (i+1) u "()"
    vizs i (Alloc t []) = (i+1, show i ++ " [label=\"new " ++ show t ++ "\"]\n")
    vizs i (Alloc t u) = (j, show i ++ " [label=\"new " ++ show t ++ "\"]\n"
                          ++ show i ++ " -> " ++ show (i+1) ++ "\n"
                          ++ idx)
        where (j,idx) = vizl (i+1) u "[]"

instance Viz Command where
    vizs i (Expr t) = vizs i t
    vizs i (Attrib t u) = (k, show i ++ " [label=\"Attrib\"]\n"
                           ++ show i ++ " -> " ++ show (i+1) ++ "\n"
                           ++ show i ++ " -> " ++ show j ++ "\n"
                           ++ lhs ++ rhs)
        where (j,lhs) = vizs (i+1) t
              (k,rhs) = vizs j u
    vizs i (Free t) = (j, show i ++ " [label=\"free\"]\n"
                       ++ show i ++ " -> " ++ show (i+1) ++ "\n"
                       ++ rhs)
        where (j,rhs) = vizs (i+1) t
    vizs i (While t u) = (k, show i ++ " [label=\"while\"]\n"
                          ++ show i ++ " -> " ++ show (i+1) ++ "\n"
                          ++ show i ++ " -> " ++ show j ++ "\n"
                          ++ ex ++ cmd)
        where (j,ex) = vizs (i+1) t
              (k,cmd) = vizl j u ";"
    vizs i (If t u) = (k, show i ++ " [label=\"if\"]\n"
                       ++ show i ++ " -> " ++ show (i+1) ++ "\n"
                       ++ show i ++ " -> " ++ show j ++ "\n"
                       ++ ex ++ cmd)
        where (j,ex) = vizs (i+1) t
              (k,cmd) = vizl j u ";"
    vizs i (IfElse t u v) = (l, show i ++ " [label=\"ifelse\"]\n"
                             ++ show i ++ " -> " ++ show (i+1) ++ "\n"
                             ++ show i ++ " -> " ++ show j ++ "\n"
                             ++ show i ++ " -> " ++ show k ++ "\n"
                             ++ ex ++ cmd1 ++ cmd2)
        where (j,ex) = vizs (i+1) t
              (k,cmd1) = vizl j u ";"
              (l,cmd2) = vizl k v ";"
    vizs i (For t u v w) = (l, show i ++ " [label=\"for " ++ t ++ "\"]\n"
                            ++ show i ++ " -> " ++ show (i+1) ++ "\n"
                            ++ show i ++ " -> " ++ show j ++ "\n"
                            ++ show i ++ " -> " ++ show k ++ "\n"
                            ++ ex1 ++ ex2 ++ cmd)
        where (j,ex1) = vizs (i+1) u
              (k,ex2) = vizs j v
              (l,cmd) = vizl k w ";"

instance Viz VarDecl where
    vizs i (VarDecl t u Nothing) = (i+2, show i ++ " [label=\"VarDecl " ++ u ++ "\"]\n"
                                      ++ show i ++ " -> " ++ show (i+1) ++ "\n"
                                      ++ show (i+1) ++ " [label=\"" ++ show t ++ "\"]\n")
    vizs i (VarDecl t u (Just v)) = (j, show i ++ " [label=\"VarDecl " ++ u ++ "\"]\n"
                                     ++ show i ++ " -> " ++ show (i+1) ++ "\n"
                                     ++ show (i+1) ++ " [label=\"" ++ show t ++ "\"]\n"
                                     ++ show i ++ " -> " ++ show (i+2) ++ "\n"
                                    ++ ex)
        where (j,ex) = vizs (i+2) v

instance Viz ParamDecl where
    vizs i (ParamDecl t u) = (i+2, show i ++ " [label=\"VarDecl " ++ u ++ "\"]\n"
                              ++ show i ++ " -> " ++ show (i+1) ++ "\n"
                              ++ show (i+1) ++ " [label=\"" ++ show t ++ "\"]\n")

instance Viz Scope where
    vizs i (Scope t u) = (k, show i ++ " [label=\"Scope\"]\n"
                          ++ show i ++ " -> " ++ show (i+1) ++ "\n"
                          ++ show i ++ " -> " ++ show j ++ "\n"
                          ++ var ++ cmd)
        where (j,var) = vizl (i+1) t ","
              (k,cmd) = vizl j u ";"

instance Viz FuncDecl where
    vizs i (FuncDecl t u v w) = (k, show i ++ " [label=\"FuncDecl " ++ u ++ "\"]\n"
                                 ++ show i ++ " -> " ++ show (i+1) ++ "\n"
                                 ++ show (i+1) ++ " [label=\"" ++ show t ++ "\"]\n"
                                 ++ show i ++ " -> " ++ show (i+2) ++ "\n"
                                 ++ show i ++ " -> " ++ show j ++ "\n"
                                 ++ par ++ sco)
        where (j,par) = vizl (i+2) v ","
              (k,sco) = vizs j w

instance Viz Program where
    vizs i (Program t u) = (k, show i ++ " [label=\"Program\"]\n"
                          ++ show i ++ " -> " ++ show (i+1) ++ "\n"
                          ++ show i ++ " -> " ++ show j ++ "\n"
                          ++ fun ++ sco)
        where (j,fun) = vizl (i+1) t ","
              (k,sco) = vizs j u