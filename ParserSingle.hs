module ParserSingle where

import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isDigit)
import Control.Applicative ((<|>))

parse :: String -> Bool
parse s = case readP_to_S prog s of
            [] -> False
            _  -> True

parseDebug :: String -> [((), String)]
parseDebug = readP_to_S prog

-- helpers

(>|) :: ReadP a -> ReadP b -> ReadP b
f >| g = f >> many (skipSpaces >> skipComments) >> g

skipComments :: ReadP ()
skipComments = optional $ skipLineComment +++ skipMultComment

skipLineComment :: ReadP String
skipLineComment = between (string "//") (string "ab") (many get)

skipMultComment :: ReadP String
skipMultComment = between (string "/*") (string "*/") (many get)

skipStart :: ReadP ()
skipStart = skipComments

-- ([a-zA-Z]|_+[a-zA-Z0-9])[a-zA-Z0-9_]*
getId :: ReadP String
getId = munch1 isAlpha <|> (munch1 ('_' ==) >> munch1 isAlpha)

-- [0-9]+
getNum :: ReadP String
getNum = munch1 isDigit

-- prods

prog :: ReadP ()
prog = skipStart >| statlist >| eof

statlist :: ReadP [String]
statlist = stat `sepBy1` string ";"

stat :: ReadP String
stat = (string "print" >| string "[" >| exprlist >| string "]")
   <|> (string "while" >| string "(" >| bexpr >| string ")" >| string "do " >| stat)
   <|> (string "conditional" >| string "[" >> caselist >> string "]" >| string "default " >| stat)
   <|> (string "{" >| statlist >| string "}")
   <|> (getId >| string ":=" >| (string "user" <|> expr))

caselist :: ReadP [String]
caselist = many caseitem

caseitem :: ReadP String
caseitem = string "case" >| string "(" >| bexpr >| string ")" >| string "do " >| stat >| (string " break" <|> return "")

bexpr :: ReadP String
bexpr = (expr  >| relop       >| expr)
    <|> (bexpr >| string "&&" >| bexpr)
    <|> (bexpr >| string "||" >| bexpr)

expr :: ReadP String
expr = (expr >| string "+" >| expr)
   <|> (expr >| string "-" >| expr)
   <|> (expr >| string "*" >| expr)
   <|> (expr >| string "/" >| expr)
   <|> (string "+" >| string "[" >| exprlist >| string "]")
   <|> (string "*" >| string "[" >| exprlist >| string "]")
   <|> getNum
   <|> getId

exprlist :: ReadP [String]
exprlist = expr `sepBy1` string ","

relop :: ReadP String
relop = string ">=" <|> string "<=" <|> string "==" <|> string ">" <|> string "<" <|> string "!="
