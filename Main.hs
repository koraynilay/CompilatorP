import Lexer

import System.Environment (getArgs, getProgName)

usage :: String -> IO String
usage s = return ("Usage: " ++ s ++ " FILE")

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> getProgName >>= usage >>= putStrLn
                as -> readAndPrintFiles as
