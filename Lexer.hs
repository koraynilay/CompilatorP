{-# LANGUAGE ViewPatterns #-}
module Lexer where

import Token

import Data.Text.Read
import Data.Text as T (pack, unpack)
import Data.Char
import Data.List

import System.Environment (getArgs, getProgName)
import System.Exit (die)

import Text.Regex.TDFA (Regex, (=~), makeRegexOpts, defaultCompOpt, defaultExecOpt)
import qualified Text.Regex.TDFA as RE (match, multiline)

stripPrefixRegex :: String -> String -> Maybe (String, String)
stripPrefixRegex re xs | mid r == "" = Nothing
                       | otherwise = Just (mid r, last r)
  where
        regex = makeRegexOpts defaultCompOpt{RE.multiline=False} defaultExecOpt ("^" ++ re) :: Regex
        r = RE.match regex xs :: (String, String, String)
        mid (x,y,z) = y
        last (x,y,z) = z

matchNumber :: String -> Either String Token
matchNumber xs = case decimal $ T.pack xs of
                        Right a -> Right $ TokenNumber Number $ fst a
                        Left e  -> Left "expected number but got non-digit characters"

matchTok :: String -> (String, Either String Token)
matchTok [] = ([], Right $ TokenSimple EOF)
matchTok (stripPrefixRegex "([a-zA-Z]|_+[a-zA-Z0-9])[a-zA-Z0-9_]*" -> Just res)
                                | matched == "print"       = (rest, Right $ TokenString Print "print")
                                | matched == "while"       = (rest, Right $ TokenString While "while")
                                | matched == "do"          = (rest, Right $ TokenString Do "do")
                                | matched == "conditional" = (rest, Right $ TokenString Conditional "conditional")
                                | matched == "case"        = (rest, Right $ TokenString Case "case")
                                | matched == "break"       = (rest, Right $ TokenString Break "break")
                                | matched == "default"     = (rest, Right $ TokenString Default "default")
                                | matched == "user"        = (rest, Right $ TokenString UserInput "user")
                                | otherwise                = (rest, Right $ TokenString Identifier matched)
  where
        (matched, rest) = res
matchTok (stripPrefixRegex "[0-9]+" -> Just res) = (snd res, matchNumber $ fst res)
matchTok (stripPrefix ":=" -> Just xs) = (xs, Right $ TokenSimple Assignment)
matchTok (stripPrefix "&&" -> Just xs) = (xs, Right $ TokenSimple Conjunction)
matchTok (stripPrefix "||" -> Just xs) = (xs, Right $ TokenSimple Disjunction)
matchTok (stripPrefix "==" -> Just xs) = (xs, Right $ TokenSimple Equal)
matchTok (stripPrefix "!=" -> Just xs) = (xs, Right $ TokenSimple NotEqual)
matchTok (stripPrefix "<=" -> Just xs) = (xs, Right $ TokenSimple LessEqual)
matchTok (stripPrefix "<"  -> Just xs) = (xs, Right $ TokenSimple LessThan)
matchTok (stripPrefix ">=" -> Just xs) = (xs, Right $ TokenSimple GreaterEqual)
matchTok (stripPrefix ">"  -> Just xs) = (xs, Right $ TokenSimple GreaterThan)
matchTok (stripPrefix "("  -> Just xs) = (xs, Right $ TokenSimple ParenOpen)
matchTok (stripPrefix ")"  -> Just xs) = (xs, Right $ TokenSimple ParenClose)
matchTok (stripPrefix "["  -> Just xs) = (xs, Right $ TokenSimple BracketOpen)
matchTok (stripPrefix "]"  -> Just xs) = (xs, Right $ TokenSimple BracketClose)
matchTok (stripPrefix "{"  -> Just xs) = (xs, Right $ TokenSimple BraceOpen)
matchTok (stripPrefix "}"  -> Just xs) = (xs, Right $ TokenSimple BraceClose)
matchTok (stripPrefix "+"  -> Just xs) = (xs, Right $ TokenSimple Plus)
matchTok (stripPrefix "-"  -> Just xs) = (xs, Right $ TokenSimple Minus)
matchTok (stripPrefix "*"  -> Just xs) = (xs, Right $ TokenSimple Multiply)
matchTok (stripPrefix "/"  -> Just xs) = (xs, Right $ TokenSimple Divide)
matchTok (stripPrefix ";"  -> Just xs) = (xs, Right $ TokenSimple Semicolon)
matchTok (stripPrefix ","  -> Just xs) = (xs, Right $ TokenSimple Comma)
matchTok (':':'3':xs) = (xs, Left $ "wwong sequence *blushes* of chawactews ^^ UwU: :3")
matchTok (x:y:xs) = (xs, Left $ "wrong sequence of characters: " ++ [x,y])
matchTok (x:[]) = ([], Left $ "wrong sequence of characters: " ++ [x])

match :: String -> [Either String Token]
match [] = [snd $ matchTok []]
match ('/':'*':xs) = finishMultilineComment xs
match ('/':'/':xs) = finishSinglelineComment xs
match (x:xs) | isSpace x = match xs
             | otherwise = (snd r) : match (fst r)
  where
        r = matchTok (x:xs)

finishMultilineComment :: String -> [Either String Token]
finishMultilineComment [] = [Left "unclosed multiline comment"]
finishMultilineComment ('*':'/':xs) = match xs
finishMultilineComment (x:xs) = finishMultilineComment xs

finishSinglelineComment :: String -> [Either String Token]
finishSinglelineComment [] = match []
finishSinglelineComment ('\n':xs) = match xs
finishSinglelineComment (x:xs) = finishSinglelineComment xs


matchIO :: String -> String -> IO [Token]
matchIO fn xs = maybeListTox fn $ match xs

maybeListTox :: String -> [Either String Token] -> IO [Token]
maybeListTox _ [] = pure []
maybeListTox fn (Left msg:_) = die $ fn ++ ": " ++ msg
maybeListTox fn (Right x:xs) = (x :) <$> maybeListTox fn xs
{- or:
maybeListTox fn (Right x:xs) = fmap ((:) x) (maybeListTox fn xs)
-}

maybeListToxUnsafe :: [Either String Token] -> [Token]
maybeListToxUnsafe [] = []
maybeListToxUnsafe (Left msg:_) = error $ "lexer error: " ++ msg
maybeListToxUnsafe (Right x:xs) = x : maybeListToxUnsafe xs

printableToken :: Token -> String
printableToken t = "<" ++ (show tok) ++ attr ++ ">"
  where
        tok = getTokenName $ tokenName t
        attr = case tokenValue t of
                Nothing -> ""
                Just a -> "," ++ either id show a

printTokens :: [Token] -> IO ()
printTokens [] = putStrLn ""
printTokens (t:ts) = putStr (printableToken t) >> putStr " " >> printTokens ts

-- this is needed because if we print the filename in matchIO
-- it will be printed after the die error message, resulting in:
-- ```output
-- error
-- filename: 
-- ```
printFilename :: String -> [Token] -> IO [Token]
printFilename xs toks = putStr (xs ++ ": ") >> return toks

usage :: String -> IO String
usage s = return ("Usage: " ++ s ++ " FILE")

readAndPrintFiles :: [String] -> IO ()
readAndPrintFiles [] = putStrLn ""
readAndPrintFiles (x:xs) = readFile x >>= matchIO x >>= printFilename x >>= printTokens >> readAndPrintFiles xs

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> getProgName >>= usage >>= putStrLn
                as -> readAndPrintFiles as
