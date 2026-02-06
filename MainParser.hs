import Lib.Token
import Lib.Parser
import Lib.LexerReadP

import System.Environment (getArgs, getProgName)

usage :: String -> IO String
usage s = return ("Usage: " ++ s ++ " FILE")

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> getProgName >>= usage >>= putStrLn
                as -> handleFiles as

handleFiles :: [String] -> IO ()
handleFiles [] = return ()
handleFiles (f:fs) = handleFile f >> handleFiles fs

handleFile :: String -> IO ()
handleFile fn = do f <- readFile fn
                   let x = lexTokens f
                   putStrLn (fn ++ ": " ++ if (parse x) then "OK" else "failed")
