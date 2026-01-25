{-# LANGUAGE ViewPatterns #-}
module Lib.Lexer where

import Lib.Token

import Data.Text.Read
import Data.Text as T (pack, unpack)
import Data.Char
import Data.List

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
                        Right a -> Right $ Number $ fst a
                        Left e  -> Left "expected number but got non-digit characters"

matchTok :: String -> (String, Either String Token)
matchTok [] = ([], Right $ EOF)
matchTok (stripPrefixRegex "([a-zA-Z]|_+[a-zA-Z0-9])[a-zA-Z0-9_]*" -> Just res)
                                | matched == "print"       = (rest, Right $ Print)
                                | matched == "while"       = (rest, Right $ While)
                                | matched == "do"          = (rest, Right $ Do)
                                | matched == "conditional" = (rest, Right $ Conditional)
                                | matched == "case"        = (rest, Right $ Case)
                                | matched == "break"       = (rest, Right $ Break)
                                | matched == "default"     = (rest, Right $ Default)
                                | matched == "user"        = (rest, Right $ UserInput)
                                | otherwise                = (rest, Right $ Identifier matched)
  where
        (matched, rest) = res
matchTok (stripPrefixRegex "[0-9]+" -> Just res) = (snd res, matchNumber $ fst res)
matchTok (stripPrefix ":=" -> Just xs) = (xs, Right $ Assignment)
matchTok (stripPrefix "&&" -> Just xs) = (xs, Right $ Conjunction)
matchTok (stripPrefix "||" -> Just xs) = (xs, Right $ Disjunction)
matchTok (stripPrefix "==" -> Just xs) = (xs, Right $ Equal)
matchTok (stripPrefix "!=" -> Just xs) = (xs, Right $ NotEqual)
matchTok (stripPrefix "<=" -> Just xs) = (xs, Right $ LessEqual)
matchTok (stripPrefix "<"  -> Just xs) = (xs, Right $ LessThan)
matchTok (stripPrefix ">=" -> Just xs) = (xs, Right $ GreaterEqual)
matchTok (stripPrefix ">"  -> Just xs) = (xs, Right $ GreaterThan)
matchTok (stripPrefix "("  -> Just xs) = (xs, Right $ ParenOpen)
matchTok (stripPrefix ")"  -> Just xs) = (xs, Right $ ParenClose)
matchTok (stripPrefix "["  -> Just xs) = (xs, Right $ BracketOpen)
matchTok (stripPrefix "]"  -> Just xs) = (xs, Right $ BracketClose)
matchTok (stripPrefix "{"  -> Just xs) = (xs, Right $ CurlyOpen)
matchTok (stripPrefix "}"  -> Just xs) = (xs, Right $ CurlyClose)
matchTok (stripPrefix "+"  -> Just xs) = (xs, Right $ Plus)
matchTok (stripPrefix "-"  -> Just xs) = (xs, Right $ Minus)
matchTok (stripPrefix "*"  -> Just xs) = (xs, Right $ Multiply)
matchTok (stripPrefix "/"  -> Just xs) = (xs, Right $ Divide)
matchTok (stripPrefix ";"  -> Just xs) = (xs, Right $ Semicolon)
matchTok (stripPrefix ","  -> Just xs) = (xs, Right $ Comma)
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
        tok = getTokenName $ t
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

readAndPrintFiles :: [String] -> IO ()
readAndPrintFiles [] = putStrLn ""
readAndPrintFiles (x:xs) = readFile x >>= matchIO x >>= printFilename x >>= printTokens >> readAndPrintFiles xs
