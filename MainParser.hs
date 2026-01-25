import Lib.Token
import Lib.Parser
import Lib.Lexer

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
handleFile fn = readFile fn >>= matchIO fn >>= \x -> putStrLn (fn ++ ": " ++ show (parse x))
--handleFile fn = do f <- readFile fn
--                   ts <- matchIO fn f
--                   print ts
--                   let r = parse ts
--                   putStrLn $ fn ++ ": " ++ show r
