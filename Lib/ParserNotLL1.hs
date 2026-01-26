module Lib.Parser where

import Lib.Token
import Lib.Lexer
import Lib.Instruction
import qualified Lib.CompilerState as CS
import Lib.CompilerState (CompilerStateT, aroundErrTok
                         , getTok, peekTok)

import Control.Applicative

parse :: [Token] -> Bool
parse ts = case CS.parse prog ts of
                Just _ -> True
                Nothing -> False

parseDebug :: [Token] -> Maybe ((), CS.CompilerState)
parseDebug ts = CS.parse prog ts

sat p = CS.sat p >> return ()
tok t = CS.tok t >> return ()
tokId = CS.tokId >> return ()
tokNum = CS.tokNum >> return ()

-- prods

prog :: CompilerStateT ()
prog = (statlist >> tok EOF)

statlist :: CompilerStateT ()
statlist = stat >> ((tok Semicolon >> statlist) <|> return ())

stat :: CompilerStateT ()
stat = (tokId >> tok Assignment >> assignv)
   <|> (tok Print >> tok BracketOpen >> exprlist >> tok BracketClose)
   <|> (tok While >> tok ParenOpen >> bexpr >> tok ParenClose >> tok Do >> stat)
   <|> (tok Conditional >> tok BracketOpen >> caselist >> tok BracketClose >> tok Default >> stat)
   <|> (tok CurlyOpen >> statlist >> tok CurlyClose)
   <|> (do t <- getTok; nt <- getTok; error ("unexpected token: " ++ show t ++ " before " ++ show nt))

assignv :: CompilerStateT ()
assignv = (tok UserInput) <|> expr

caselist :: CompilerStateT ()
caselist = caseitem >> (caselist <|> return ())

caseitem :: CompilerStateT ()
caseitem = tok Case >> tok ParenOpen >> bexpr >> tok ParenClose >> tok Do >> stat >> (tok Break <|> return ())

bexpr :: CompilerStateT ()
bexpr = (relop >> expr >> expr)
    <|> (tok Conjunction >> bexpr >> bexpr)
    <|> (tok Disjunction >> bexpr >> bexpr)
    <|> (do t <- getTok; nt <- getTok; error ("unexpected token: " ++ show t ++ " before " ++ show nt))

expr :: CompilerStateT ()
expr = (tok Plus >> operands)
   <|> (tok Minus >> expr >> expr)
   <|> (tok Multiply >> operands)
   <|> (tok Divide >> expr >> expr)
   <|> (tokNum)
   <|> (tokId)
   <|> (do t <- getTok; nt <- getTok; error ("unexpected token: " ++ show t ++ " before " ++ show nt))

operands :: CompilerStateT ()
operands = (expr >> expr)
       <|> (tok BracketOpen >> exprlist >> tok BracketClose)

exprlist :: CompilerStateT ()
exprlist = expr >> ((tok Comma >> exprlist) <|> return ())

relop :: CompilerStateT ()
relop = (tok GreaterEqual)
    <|> (tok LessEqual)
    <|> (tok Equal)
    <|> (tok GreaterThan)
    <|> (tok LessThan)
    <|> (tok NotEqual)
    <|> (do t <- getTok; nt <- getTok; error ("unexpected token: " ++ show t ++ " before " ++ show nt))
