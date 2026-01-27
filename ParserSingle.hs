module ParserSingle where

import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isDigit, isSpace)
import Control.Applicative ((<|>))
import Control.Monad (void)

parse :: String -> Bool
parse s = case readP_to_S prog s of
            [] -> False
            _  -> True

parseDebug :: String -> [((), String)]
parseDebug = readP_to_S prog

-- helpers

-- like (>>) but skipping all spaces and comments
(>|) :: ReadP a -> ReadP b -> ReadP b
f >| g = f >> skipSpaces >> many (skipComments >> skipSpaces) >> g
infixl 1 >|

-- like (>|) but requiring at least 1 space before skipping
(>!) :: ReadP a -> ReadP b -> ReadP b
f >! g = f >> optional skipComments >> munch1 isSpace >| g
infixl 1 >!

skipComments :: ReadP String
skipComments = skipLineComment +++ skipMultComment

skipLineComment :: ReadP String
skipLineComment = string "//" >> manyTill get (void (char '\n') +++ eof)

skipMultComment :: ReadP String
skipMultComment = string "/*" >> manyTill get (string "*/")

-- ([a-zA-Z]|_+[a-zA-Z0-9])[a-zA-Z0-9_]*
getId :: ReadP String
getId = do c <- (satisfy isAlpha >>= \c -> return [c]) +++ munch1 ('_' ==)
           x <- many (satisfy isAlpha +++ satisfy isDigit +++ satisfy ('_' ==))
           return $ c ++ x

-- [0-9]+
getNum :: ReadP String
getNum = munch1 isDigit

-- prods

prog :: ReadP ()
prog = return () >| statlist >| eof

statlist :: ReadP [String]
statlist = stat >| many (string ";" >| stat)

stat :: ReadP String
stat = (string "print" >| string "[" >| exprlist >| string "]")
   <|> (string "while" >| string "(" >| bexpr >| string ")" >| string "do" >! stat)
   <|> (string "conditional" >| string "[" >| caselist >| string "]" >| string "default" >! stat)
   <|> (string "{" >| statlist >| string "}")
   <|> (getId >| string ":=" >| (string "user" <|> expr))

caselist :: ReadP [String]
caselist = caseitem >| (caselist <|> return ["a"])

caseitem :: ReadP String
caseitem = string "case" >| string "(" >| bexpr >| string ")" >| string "do" >! stat >! (string "break" <|> return "")

bexpr :: ReadP String
bexpr = (relop >| expr >! expr)
    <|> (string "&&" >| bexpr >| bexpr)
    <|> (string "||" >| bexpr >| bexpr)

expr :: ReadP String
expr = (string "+" >| expr >! expr)
   <|> (string "-" >| expr >! expr)
   <|> (string "*" >| expr >! expr)
   <|> (string "/" >| expr >! expr)
   <|> (string "+" >| string "[" >| exprlist >| string "]")
   <|> (string "*" >| string "[" >| exprlist >| string "]")
   <|> getNum
   <|> getId

exprlist :: ReadP [String]
exprlist = expr >| many (string "," >| expr)

relop :: ReadP String
relop = string ">=" <|> string "<=" <|> string "==" <|> string ">" <|> string "<" <|> string "!="
