module LexerReadP where

import Lib.Token

import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isDigit, isSpace)
import Control.Applicative ((<|>))
import Control.Monad (void)

lex :: String -> [Token]
lex = concat.map fst.(readP_to_S match)

lexDebug :: String -> [([Token], String)]
lexDebug = readP_to_S match

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

match :: ReadP [Token]
match = do ts <- many1 matchTok
           eof
           return $ ts ++ [EOF]

matchTok :: ReadP Token
matchTok = (string "print"       >| return Print)
       <|> (string "while"       >| return While)
       <|> (string "do"          >| return Do)
       <|> (string "conditional" >| return Conditional)
       <|> (string "case"        >| return Case)
       <|> (string "break"       >| return Break)
       <|> (string "default"     >| return Default)
       <|> (string "user"        >| return UserInput)
       <|> (Identifier <$> getId)
       <|> (Number . read <$> getNum)
       <|> (string ":=" >| return Assignment)
       <|> (string "&&" >| return Conjunction)
       <|> (string "||" >| return Disjunction)
       <|> (string "==" >| return Equal)
       <|> (string "!=" >| return NotEqual)
       <|> (string "<=" >| return LessEqual)
       <|> (string ">=" >| return GreaterEqual)
       <|> (string "<" >| return LessThan)
       <|> (string ">" >| return GreaterThan)
       <|> (string "(" >| return ParenOpen)
       <|> (string ")" >| return ParenClose)
       <|> (string "[" >| return BracketOpen)
       <|> (string "]" >| return BracketClose)
       <|> (string "{" >| return CurlyOpen)
       <|> (string "}" >| return CurlyClose)
       <|> (string "+" >| return Plus)
       <|> (string "-" >| return Minus)
       <|> (string "*" >| return Multiply)
       <|> (string "/" >| return Divide)
       <|> (string ";" >| return Semicolon)
       <|> (string "," >| return Comma)
