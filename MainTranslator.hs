import Lib.Token
import Lib.Translator
import Lib.Lexer
import Lib.CodeGen (toJasmin)

import System.Environment (getArgs, getProgName)
import Control.DeepSeq

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
                   ts <- matchIO fn f
                   let r = parse ts
                   putStr $!! toJasmin r
