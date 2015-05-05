module Parser where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<*>),(<*),(*>),(<$>))

import AST

whitespace :: Parser ()
whitespace = space *> spaces

word :: String -> Parser String
word w = string w <* notFollowedBy (alphaNum <|> char '_')

oneOfStr :: [String] -> Parser String
oneOfStr (x:xs) = foldl (<|>) (try $ string x) (map (try . string) xs)

intlit :: Parser Expr
intlit = IntLit <$> (read <$> many1 digit)

boollit :: Parser Expr
boollit = BoolLit <$> (word "Verdadeiro" <|> word "Falso")

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

lbinop :: [String] -> Parser Expr -> Parser Expr
lbinop ops unit = foldl (\e (a,b)->BinOp e a b) <$> unit
                        <*> many ((\a b->(a,b)) <$> (oneOfStr ops <?> "operator")
                                                <*> unit)

alloc :: Parser Expr
alloc = Alloc <$> (try (word "novo") *> whitespace *> typename <* spaces)
              <*> many1 (between (char '[') (char ']' *> spaces) expr)

var :: Parser Expr
var = Var <$> nameid <* spaces <*> many (between (char '[') (char ']' *> spaces) expr)

negation :: Parser Expr
negation = Not <$> (char '!' *> spaces *> term)

call :: Parser Expr
call = Func <$> nameid <* spaces <*> between (char '(') (char ')') (expr `sepBy` (char ','))

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
while = While <$> (try (word "enquanto") *> spaces
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
free = Free <$> (try (word "free") *> whitespace *> expr)

for :: Parser Command
for =  For <$> (try (word "itere") *> spaces
            *> char '(' *> spaces *> (var <* spaces <* char ':' <* spaces) <* spaces)
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
