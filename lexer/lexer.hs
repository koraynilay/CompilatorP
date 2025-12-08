{-# LANGUAGE ViewPatterns #-}
import Data.Text.Read
import Data.Text as T (pack, unpack)
import Data.Char
import Data.List
import Data.Either

import System.Environment (getArgs, getProgName)

import Text.Regex.TDFA ((=~))

stripPrefixRegex :: String -> String -> Maybe (String, String)
stripPrefixRegex re xs | mid r == "" = Nothing
                 | otherwise = Just (mid r, last r)
  where
        r = xs =~ ("^" ++ re) :: (String, String, String)
        mid (x,y,z) = y
        last (x,y,z) = z

-- TokenName            Pattern                         Nome
-- -------------------------------------------------------
-- Numeri           Costante numerica                   256
-- Identificatore   Lettera seguita da lettere e cifre  257
-- Print            print                               258
-- While            while                               259
-- Do               do                                  260
-- Conditional      conditional                         261
-- Case             case                                262
-- Break            break                               263
-- Default          default                             264
-- Input utente     user                                265
-- Assegnamento     :=                                  266
-- Congiunzione     &&                                  267
-- Disgiunzione     ||                                  268
-- Minore/uguale    <=                                  269
-- Maggiore/uguale  >=                                  270
-- Uguale a         ==                                  271
-- Diverso da       !=                                  272
-- Minore di        <                                   60
-- Maggiore di      >                                   62
-- Par. tonda (     (                                   40
-- Par. tonda )     )                                   41
-- Par. quadra [    [                                   91
-- Par. quadra ]    ]                                   93
-- Par. graffa {    {                                   123
-- Par. graffa }    }                                   125
-- Somma            +                                   43
-- Sottrazione      -                                   45
-- Moltiplicaz.     *                                   42
-- Divisione        /                                   47
-- Punto e virgola  ;                                   59
-- Virgola          ,                                   44
-- EOF              Fine input                          -1

data TokenName = Number        | Identifier   | Print        | While
               | Do            | Conditional  | Case         | Break
               | Default       | UserInput    | Assignment   | Conjunction
               | Disjunction   | LessEqual    | GreaterEqual | Equal
               | NotEqual      | LessThan     | GreaterThan  | ParenOpen
               | ParenClose    | BracketOpen  | BracketClose | BraceOpen
               | BraceClose    | Plus         | Minus        | Multiply
               | Divide        | Semicolon    | Comma        | EOF
                deriving (Show, Eq)

getTokenName :: TokenName -> Int
getTokenName Number        = 256
getTokenName Identifier    = 257
getTokenName Print         = 258
getTokenName While         = 259
getTokenName Do            = 260
getTokenName Conditional   = 261
getTokenName Case          = 262
getTokenName Break         = 263
getTokenName Default       = 264
getTokenName UserInput     = 265
getTokenName Assignment    = 266
getTokenName Conjunction   = 267
getTokenName Disjunction   = 268
getTokenName LessEqual     = 269
getTokenName GreaterEqual  = 270
getTokenName Equal         = 271
getTokenName NotEqual      = 272
getTokenName LessThan      = 60
getTokenName GreaterThan   = 62
getTokenName ParenOpen     = 40
getTokenName ParenClose    = 41
getTokenName BracketOpen   = 91
getTokenName BracketClose  = 93
getTokenName BraceOpen     = 123
getTokenName BraceClose    = 125
getTokenName Plus          = 43
getTokenName Minus         = 45
getTokenName Multiply      = 42
getTokenName Divide        = 47
getTokenName Semicolon     = 59
getTokenName Comma         = 44
getTokenName EOF           = -1

data Token = TokenString TokenName String
           | TokenNumber TokenName Integer
           | TokenSimple TokenName
             deriving (Show, Eq)

tokenName :: Token -> TokenName
tokenName (TokenString n _) = n
tokenName (TokenNumber n _) = n
tokenName (TokenSimple n)   = n

tokenValue :: Token -> Maybe (Either String Integer)
tokenValue (TokenString _ s) = Just $ Left s
tokenValue (TokenNumber _ n) = Just $ Right n
tokenValue (TokenSimple n)   = Nothing

matchNumber :: String -> Token
matchNumber xs = TokenNumber Number $ fst $ fromRight (0, T.pack "err") $ decimal $ T.pack xs

matchTok :: String -> (String, Token)
matchTok [] = ([], TokenSimple EOF)
matchTok (stripPrefixRegex "[a-zA-Z][a-zA-Z0-9]*" -> Just res)
                                | matched == "print"       = (rest, TokenString Print "print")
                                | matched == "while"       = (rest, TokenString While "while")
                                | matched == "do"          = (rest, TokenString Do "do")
                                | matched == "conditional" = (rest, TokenString Conditional "conditional")
                                | matched == "case"        = (rest, TokenString Case "case")
                                | matched == "break"       = (rest, TokenString Break "break")
                                | matched == "default"     = (rest, TokenString Default "default")
                                | matched == "user"        = (rest, TokenString UserInput "user")
                                | otherwise                = (rest, TokenString Identifier matched)
  where
        (matched, rest) = res
matchTok (stripPrefixRegex "[0-9]+" -> Just res) = (snd res, matchNumber $ fst res)
matchTok (stripPrefix ":=" -> Just xs) = (xs, TokenSimple Assignment)
matchTok (stripPrefix "&&" -> Just xs) = (xs, TokenSimple Conjunction)
matchTok (stripPrefix "||" -> Just xs) = (xs, TokenSimple Disjunction)
matchTok (stripPrefix "==" -> Just xs) = (xs, TokenSimple Equal)
matchTok (stripPrefix "!=" -> Just xs) = (xs, TokenSimple NotEqual)
matchTok (stripPrefix "<=" -> Just xs) = (xs, TokenSimple LessEqual)
matchTok (stripPrefix "<"  -> Just xs) = (xs, TokenSimple LessThan)
matchTok (stripPrefix ">=" -> Just xs) = (xs, TokenSimple GreaterEqual)
matchTok (stripPrefix ">"  -> Just xs) = (xs, TokenSimple GreaterThan)
matchTok (stripPrefix "("  -> Just xs) = (xs, TokenSimple ParenOpen)
matchTok (stripPrefix ")"  -> Just xs) = (xs, TokenSimple ParenClose)
matchTok (stripPrefix "["  -> Just xs) = (xs, TokenSimple BracketOpen)
matchTok (stripPrefix "]"  -> Just xs) = (xs, TokenSimple BracketClose)
matchTok (stripPrefix "{"  -> Just xs) = (xs, TokenSimple BraceOpen)
matchTok (stripPrefix "}"  -> Just xs) = (xs, TokenSimple BraceClose)
matchTok (stripPrefix "+"  -> Just xs) = (xs, TokenSimple Plus)
matchTok (stripPrefix "-"  -> Just xs) = (xs, TokenSimple Minus)
matchTok (stripPrefix "*"  -> Just xs) = (xs, TokenSimple Multiply)
matchTok (stripPrefix "/"  -> Just xs) = (xs, TokenSimple Divide)
matchTok (stripPrefix ";"  -> Just xs) = (xs, TokenSimple Semicolon)
matchTok (stripPrefix ","  -> Just xs) = (xs, TokenSimple Comma)

match :: String -> [Token]
match [] = [snd $ matchTok []]
match (x:xs) | isSpace x = match xs
             | otherwise = (snd r) : match (fst r)
  where
        r = matchTok (x:xs)

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

usage :: String -> IO String
usage s = return ("Usage: " ++ s ++ " FILE")

readAndPrintFiles :: [String] -> IO ()
readAndPrintFiles [] = putStrLn ""
readAndPrintFiles (x:xs) = putStr (x ++ ": ") >> readFile x >>= printTokens . match >> readAndPrintFiles xs

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> getProgName >>= usage >>= putStrLn
                as -> readAndPrintFiles as
