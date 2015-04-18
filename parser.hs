import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<*>),(<*),(*>),(<$>))

type Nameid = String

data Typeid     = Int
                | Bool
                | String
                | Vector     Typeid
   deriving Read

data Program    = Program   [FuncDecl] Scope                        deriving Show
data Scope      = Scope     [VarDecl] [Command]                     deriving Show

data FuncDecl   = FuncDecl   Typeid    Nameid   [ParamDecl] Scope   deriving Show
data ParamDecl  = ParamDecl  Typeid    Nameid                       deriving Show
data VarDecl    = VarDecl    Typeid    Nameid   (Maybe Expr)        deriving Show

data Command    = While      Expr     [Command]
                | If         Expr     [Command]
                | IfElse     Expr     [Command] [Command]
                | For        Nameid    Expr      Expr      [Command]
                | Free       Nameid
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
    show (Alloc t e) = "novo " ++ show t ++ (foldl (\a x->a ++ '[' : (show x) ++ "]") "" e)
    show (Func n p) = n ++ '(' : (tail . init . show) p ++ ")"

whitespace :: Parser ()
whitespace = space *> spaces

word :: String -> Parser String
word w = string w <* notFollowedBy (alphaNum <|> char '_')

oneOfStr :: [String] -> Parser String
oneOfStr (x:xs) = foldl (<|>) (try $ string x) (map (try . string) xs)

intlit :: Parser Expr
intlit = IntLit <$> (read <$> many1 digit)

boollit :: Parser Expr
boollit = BoolLit <$> (read <$> (word "True" <|> word "False"))

stringlit :: Parser Expr
stringlit = StringLit <$> between (char '"') (char '"') (many $ noneOf "\"\0\n")

nameid :: Parser String
nameid = (:) <$> (letter <|> char '_') <*> many (alphaNum <|> char '_')

typeid :: Parser Typeid
typeid = typevec <$> typename <* spaces
                 <*> (length <$> many (between (char '[') (char ']') spaces <* spaces))

typename :: Parser Typeid
typename = read <$> (word "Int" <|> word "Bool" <|> word "String")

typevec t d | d > 0     = Vector (typevec t (d - 1))
            | otherwise = t

expr :: Parser Expr
expr = ( lbinop [">=",">","<=","<","!=","=="]  -- Menor precedência
       . lbinop ["&","|","^"]
       . lbinop ["+","-"]
       . lbinop ["*","/"]                      -- Maior precedência
       ) $ (between spaces spaces term <?> "expression")

alloc :: Parser Expr
alloc = Alloc <$> (try (word "new") *> whitespace *> typename <* spaces)
              <*> many1 (between (char '[') (char ']' *> spaces) expr)

var :: Parser Expr
var = Var <$> nameid <* spaces <*> many (between (char '[') (char ']' *> spaces) expr)

negation :: Parser Expr
negation = Not <$> (char '!' *> spaces *> term)

call :: Parser Expr
call = Func <$> nameid <* spaces <*> between (char '(') (char ')') (expr `sepBy` (char ','))

lbinop :: [String] -> Parser Expr -> Parser Expr
lbinop ops unit = foldl (\e (a,b)->BinOp e a b) <$> unit
                        <*> many ((\a b->(a,b)) <$> (oneOfStr ops <?> "operator")
                                                <*> unit)

term :: Parser Expr
term = between (char '(') (char ')') expr
   <|> stringlit
   <|> boollit
   <|> intlit
   <|> negation
   <|> alloc
   <|> try call
   <|> var

block :: Parser [Command]
block = between (char '{') (char '}') (spaces *> many (command <* spaces))

command :: Parser Command
command = while
      <|> try ifelse
      <|> ifthen
      <|> for
      <|> free <* spaces <* char ';'
      <|> try attrib <* char ';'
      <|> Expr <$> expr <* char ';'

while :: Parser Command
while = While <$> (try (word "while") *> spaces
               *> between (char '(') (char ')') expr <* spaces)
              <*> block

ifelse :: Parser Command
ifelse = IfElse <$> (try (word "se") *> spaces
                 *> between (char '(') (char ')') expr <* spaces)
                <*> (word "entao" *> spaces *> block <* spaces)
                <*> (word "senao" *> spaces *> block)

ifthen :: Parser Command
ifthen = If <$> (try (word "se") *> spaces
             *> between (char '(') (char ')') expr <* spaces)
            <*> (word "entao" *> spaces *> block)

attrib :: Parser Command
attrib = Attrib <$> (var <* spaces <* string "<-") <*> expr

free :: Parser Command
free = Free <$> (try (word "free") *> whitespace *> nameid)

for :: Parser Command
for =  For <$> (try (word "itere") *> spaces
            *> char '(' *> spaces *> (nameid <* spaces <* char ':' <* spaces) <* spaces)
           <*> (expr <* string ".." <* spaces)
           <*> (expr <* char ')' <* spaces)
           <*> block

vardecl :: Parser VarDecl
vardecl = VarDecl <$> typeid <* spaces <*> nameid <* spaces
                  <*> optionMaybe (string "<-" *> spaces *> expr)

paramdecl :: Parser ParamDecl
paramdecl = ParamDecl <$> typeid <* spaces <*> nameid

vars :: Parser [VarDecl]
vars = try (word "vars") *> spaces *> char ':' *> spaces
    *> vardecl `sepBy` (spaces *> char ',' *> spaces) <* char ';'
   <|> return []

scope :: Parser Scope
scope = Scope <$> (char '{' *> spaces *> vars <* spaces)
              <*> many (command <* spaces) <* char '}'

funcdecl :: Parser FuncDecl
funcdecl = FuncDecl <$> typeid <* spaces <*> nameid <* spaces
                    <*> between (char '(' *> spaces) (spaces *> char ')' *> spaces)
                                (spaces *> (paramdecl <* spaces) `sepBy` (char ',' *> spaces))
                    <*> scope

program :: Parser Program
program = Program <$> many (funcdecl <* spaces)
                  <*> (string "programa" *> spaces *> scope)

portuga :: Parser Program
portuga = spaces *> program <* spaces <* eof
