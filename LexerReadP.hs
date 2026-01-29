module LexerReadP where

import Lib.Token

import Text.ParserCombinators.ReadP
import Data.Char (isAlphaNum, isAlpha, isDigit, isSpace)
import Control.Applicative ((<|>))
import Control.Monad (void)

lex :: String -> [Token]
lex s = case [ toks | (toks, "") <- readP_to_S match s ] of
          [t]      -> t
          ts@(t:_) -> error ("more than one possible parsing: " ++ show ts)
          _        -> error ("no parsing")
--lex = concat.map fst.(readP_to_S match)

lexDebug :: String -> [([Token], String)]
lexDebug = readP_to_S match

-- helpers

-- like (>>) but skipping all spaces and comments
(>|) :: ReadP a -> ReadP b -> ReadP b
f >| g = f >> skipWhite >> g
infixl 1 >|

-- like (>|) but requiring at least 1 space before skipping
(>!) :: ReadP a -> ReadP b -> ReadP b
f >! g = f >> skipWhite1 >| g
infixl 1 >!

skipWhite :: ReadP ()
skipWhite = skipSpaces >> (skipComments >> skipWhite) <++ return ()

skipWhite1 :: ReadP ()
skipWhite1 = optional skipComments >> munch1 isSpace >> return ()

skipComments :: ReadP ()
skipComments = (skipLineComment +++ skipMultComment) >> return ()

skipLineComment :: ReadP String
skipLineComment = string "//" >> manyTill get (void (char '\n') +++ eof)

skipMultComment :: ReadP String
skipMultComment = string "/*" >> manyTill get (string "*/")

getSkip :: ReadP String -> ReadP String
getSkip lexer = do s <- lexer
                   skipWhite
                   return s

-- ([a-zA-Z]|_+[a-zA-Z0-9])[a-zA-Z0-9_]*
getId :: ReadP String
getId = do c <- (do c <- satisfy isAlpha
                    return [c])
                +++ (do u <- munch1 ('_' ==)
                        c <- satisfy isAlphaNum
                        return $ u ++ [c])
           x <- munch (\c -> isAlphaNum c || '_' == c)
           return $ c ++ x

-- [0-9]+
getNum :: ReadP String
getNum = munch1 isDigit

-- prods

match :: ReadP [Token]
match = do skipWhite
           ts <- many matchTok
           eof
           return $ ts ++ [EOF]

matchTok :: ReadP Token
matchTok = (do s <- getSkip getId
               case s of
                 "print"       -> return Print
                 "while"       -> return While
                 "do"          -> return Do
                 "conditional" -> return Conditional
                 "case"        -> return Case
                 "break"       -> return Break
                 "default"     -> return Default
                 "user"        -> return UserInput
                 var           -> return (Identifier var))
       <++ (Number . read <$> (getSkip getNum))
       <++ (string ":=" >| return Assignment)
       <++ (string "&&" >| return Conjunction)
       <++ (string "||" >| return Disjunction)
       <++ (string "==" >| return Equal)
       <++ (string "!=" >| return NotEqual)
       <++ (string "<=" >| return LessEqual)
       <++ (string ">=" >| return GreaterEqual)
       <++ (string "<" >| return LessThan)
       <++ (string ">" >| return GreaterThan)
       <++ (string "(" >| return ParenOpen)
       <++ (string ")" >| return ParenClose)
       <++ (string "[" >| return BracketOpen)
       <++ (string "]" >| return BracketClose)
       <++ (string "{" >| return CurlyOpen)
       <++ (string "}" >| return CurlyClose)
       <++ (string "+" >| return Plus)
       <++ (string "-" >| return Minus)
       <++ (string "*" >| return Multiply)
       <++ (string "/" >| return Divide)
       <++ (string ";" >| return Semicolon)
       <++ (string "," >| return Comma)
       <++ (do cc <- count 2 get
               error $ "wrong sequence of characters: " ++ cc)
