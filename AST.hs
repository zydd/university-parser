module AST where

type Nameid = String

data Typeid     = Int
                | Bool
                | String
                | Vector     Typeid
   deriving Read

data Program    = Program   [FuncDecl] Scope                              deriving Show
data Scope      = Scope     [VarDecl] [Command]                           deriving Show

data FuncDecl   = FuncDecl   Typeid    Nameid   [ParamDecl]       Scope   deriving Show
data ParamDecl  = ParamDecl  Typeid    Nameid                             deriving Show
data VarDecl    = VarDecl    Typeid    Nameid   (Maybe Expr)              deriving Show

data Command    = While      Expr     [Command]
                | If         Expr     [Command] (Maybe [Command])
                | For        Expr      Expr      Expr            [Command]
                | Free       Expr
                | Attrib     Expr      Expr
                | Expr       Expr
   deriving Show

data Expr       = IntLit     Int
                | BoolLit    Bool
                | StringLit  String
                | Var        Nameid   [Expr]
                | Not        Expr
                | BinOp      Expr      String    Expr  -- Operador binário associativo à esquerda
                | Func       Nameid   [Expr]
                | Alloc      Typeid   [Expr]
                | Tern       Expr      Expr      Expr

instance Show Typeid where
    show Int = "Int"
    show Bool = "Bool"
    show String = "String"
    show (Vector t) = (show t) ++ "[]"

instance Show Expr where
    show (IntLit i) = show i
    show (BoolLit i) = show i
    show (StringLit i) = show i
    show (BinOp a o b) = '(' : (show a) ++ o ++ (show b) ++ ")"
    show (Var n e) = n ++ (foldl (\a x->a ++ '[' : (show x) ++ "]") "" e)
    show (Not e) = "!(" ++ show e ++ ")"
    show (Alloc t e) = "new " ++ show t ++ (foldl (\a x->a ++ '[' : (show x) ++ "]") "" e)
    show (Func n p) = n ++ '(' : (tail . init . show) p ++ ")"
    show (Tern c t f) = '(' : show c ++ ")? (" ++ show t ++ "): (" ++ show f ++ ")"

