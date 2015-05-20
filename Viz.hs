module Viz where

import AST

class Viz a where
    viz :: a -> String
    viz a = let (_,g) = vizs 0 a in "digraph {\n" ++ g ++ "}\n"
    vizs :: Int -> a -> (Int,String)

a --> b = show a ++ " -> " ++ show b ++ "\n"
a `iarrow` b = show a ++ " -> " ++ show b ++ " [style=invis]\n"
a `label` b = show a ++ " [label=\"" ++ b ++ "\"]\n"

rank (_:[]) = ""
rank x = "{rank=same; " ++ foldr1 (++) (map ((++";") . show) x) ++ "}\n"

order x = rank x ++ chain x
    where chain (x:[]) = ""
          chain (x:y:xs) = x `iarrow` y ++ order (y:xs)

vizl i ([]) p = (i+1, i `label` p)
vizl i xs p = vizls i xs p []

vizls i (x:[]) p c = (k, i `label` p
                      ++ i --> (i+1)
                      ++ rank (i:c)
                      ++ ex)
    where (k,ex) = vizs (i+1) x

vizls i (x:xs) p c = (l, i `label` p
                      ++ i --> (i+1)
                      ++ i --> (k)
                      ++ ex ++ ex2)
    where (k,ex) = vizs (i+1) x
          (l,ex2) = vizls k xs p (i:c)

instance Viz Expr where
    vizs i (IntLit t) = (i+1,i `label` (show t))

    vizs i (BoolLit t) = (i+1,i `label` (show t))

    vizs i (StringLit t) = (i+1,i `label` ("\\\"" ++ t ++ "\\\""))

    vizs i (Not t) = let (j,rhs) = vizs (i+1) t
                      in (j, i `label` ("!")
                          ++ i --> (i+1)
                          ++ rhs)

    vizs i (BinOp t o u) = let (j,lhs) = vizs (i+1) t
                               (k,rhs) = vizs j u
                           in (k, i `label` (o)
                               ++ i --> (i+1)
                               ++ i --> j
                               ++ lhs ++ rhs
                               ++ order [i+1,j])

    vizs i (Var t []) = (i+2,i `label` "Var"
                          ++ (i+1) `label` t
                          ++ i --> (i+1))

    vizs i (Var t u) = let (j,idx) = vizl (i+2) u "[]"
                        in (j, i `label` "Var"
                            ++ (i+1) `label` t
                            ++ i --> (i+1)
                            ++ i --> (i+2)
                            ++ idx)

    vizs i (Func t []) = (i+2, i `label` "Func"
                            ++ (i+1) `label` t
                            ++ i --> (i+1))

    vizs i (Func t u) = let (j,idx) = vizl (i+2) u "()"
                        in (j, i `label` "Func"
                            ++ (i+1) `label` t
                            ++ i --> (i+1)
                            ++ i --> (i+2)
                            ++ idx
                            ++ order [i+1,i+2])

    vizs i (Alloc t []) = (i+2, i `label` "new"
                             ++ (i+1) `label` (show t)
                             ++ i --> (i+1))

    vizs i (Alloc t u) = let (j,idx) = vizl (i+2) u "[]"
                          in (j, i `label` "new"
                           ++ (i+1) `label` (show t)
                           ++ i --> (i+1)
                           ++ i --> (i+2)
                           ++ idx
                           ++ order [i+1,i+2])

    vizs i (Tern t u v) = let (j,ex1) = vizs (i+1) t
                              (k,ex2) = vizs j u
                              (l,ex3) = vizs k u
                           in (l, i `label` "?"
                               ++ i --> (i+1)
                               ++ i --> j
                               ++ i --> k
                               ++ ex1 ++ ex2 ++ ex3
                               ++ order [i+1,j,k])

instance Viz Command where
    vizs i (Expr t) = vizs i t

    vizs i (Attrib t u) = let (j,lhs) = vizs (i+1) t
                              (k,rhs) = vizs j u
                           in (k, i `label` "Attrib"
                               ++ i --> (i+1)
                               ++ i --> j
                               ++ lhs ++ rhs
                               ++ order [i+1,j])

    vizs i (Free t) = let (j,rhs) = vizs (i+1) t
                       in (j, i `label` "free"
                           ++ i --> (i+1)
                           ++ rhs)

    vizs i (While t (Scope _ u)) = let (j,ex) = vizs (i+1) t
                                       (k,cmd) = vizl j u ";"
                                   in (k, i `label` "while"
                                       ++ i --> (i+1)
                                       ++ i --> j
                                       ++ ex ++ cmd
                                       ++ order [i+1,j])

    vizs i (If t (Scope _ u) Nothing) = let (j,ex) = vizs (i+1) t
                                            (k,cmd) = vizl j u ";"
                              in (k, i `label` ("if")
                                  ++ i --> (i+1)
                                  ++ i --> j
                                  ++ ex ++ cmd
                                  ++ order [i+1,j])

    vizs i (If t (Scope _ u) (Just (Scope _ v))) =
        let (j,ex) = vizs (i+1) t
            (k,cmd1) = vizl j u ";"
            (l,cmd2) = vizl k v ";"
        in (l, i `label` "ifelse"
            ++ i --> (i+1)
            ++ i --> j
            ++ i --> k
            ++ ex ++ cmd1 ++ cmd2
            ++ order [i+1,j,k])

    vizs i (For t u v (Scope _ w)) = let (j,var) = vizs (i+1) t
                                         (k,ex1) = vizs j u
                                         (l,ex2) = vizs k v
                                         (m,cmd) = vizl l w ";"
                                      in (m, i `label` "for "
                                          ++ i --> (i+1)
                                          ++ i --> j
                                          ++ i --> k
                                          ++ i --> l
                                          ++ var ++ ex1 ++ ex2 ++ cmd
                                          ++ order [i+1,j,k,l])

instance Viz VarDecl where
    vizs i (VarDecl t u Nothing) = (i+3, i `label` "VarDecl"
                                      ++ (i+1) `label` (show t)
                                      ++ (i+2) `label` u
                                      ++ i --> (i+1)
                                      ++ i --> (i+2)
                                      ++ order [i+1,i+2])

    vizs i (VarDecl t u (Just v)) = let (j,ex) = vizs (i+3) v
                                     in (j, i `label` "VarDecl"
                                     ++ (i+1) `label` (show t)
                                     ++ (i+2) `label` u
                                     ++ i --> (i+1)
                                     ++ i --> (i+2)
                                     ++ i --> (i+3)
                                     ++ ex
                                     ++ order [i+1,i+2,i+3])

instance Viz ParamDecl where
    vizs i (ParamDecl t u) = (i+3, i `label` ("ParDecl")
                                ++ (i+1) `label` (show t)
                                ++ (i+2) `label` u
                                ++ i --> (i+1)
                                ++ i --> (i+2)
                                ++ order [i+1,i+2])

instance Viz Scope where
    vizs i (Scope t u) = let (j,var) = vizl (i+1) t ","
                             (k,cmd) = vizl j u ";"
                          in (k, i `label` "Scope"
                              ++ i --> (i+1)
                              ++ i --> j
                              ++ var ++ cmd
                              ++ order [i+1,j])

instance Viz FuncDecl where
    vizs i (FuncDecl t u v w) = let (j,par) = vizl (i+3) v ","
                                    (k,sco) = vizs j w
                                 in (k, i `label` "FuncDecl"
                                     ++ (i+1) `label` (show t)
                                     ++ (i+2) `label` u
                                     ++ i --> (i+1)
                                     ++ i --> (i+2)
                                     ++ i --> (i+3)
                                     ++ i --> j
                                     ++ par ++ sco
                                     ++ order [i+1,i+2,i+3,j])

instance Viz Program where
    vizs i (Program t u) = let (j,fun) = vizl (i+1) t ","
                               (k,sco) = vizs j u
                            in (k, i `label` "Program"
                                ++ i --> (i+1)
                                ++ i --> j
                                ++ fun ++ sco)

