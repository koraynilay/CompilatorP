module Lib.Parser where

import Lib.Token
import Lib.Lexer
import Lib.Instruction
import qualified Lib.CompilerState as CS
import Lib.CompilerState (CompilerStateT
                         , getTok, peekTok)

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
statlist = stat >> peekTok >>= \t -> case t of
                   Semicolon -> tok Semicolon >> statlist
                   _         -> return ()

stat :: CompilerStateT ()
stat = peekTok >>= \t -> case t of
       Identifier _ -> tokId >> tok Assignment >> assignv
       Print        -> tok Print >> tok BracketOpen >> exprlist >> tok BracketClose
       While        -> tok While >> tok ParenOpen >> bexpr >> tok ParenClose >> tok Do >> stat
       Conditional  -> tok Conditional >> tok BracketOpen >> caselist >> tok BracketClose >> tok Default >> stat
       CurlyOpen    -> tok CurlyOpen >> statlist >> tok CurlyClose
       _            -> error ("unexpected token: " ++ show t)

assignv :: CompilerStateT ()
assignv = peekTok >>= \t -> case t of
          UserInput -> tok UserInput
          _         -> expr

caselist :: CompilerStateT ()
caselist = caseitem >> peekTok >>= \t -> case t of
                       Case -> caselist
                       _    -> return ()

caseitem :: CompilerStateT ()
caseitem = tok Case >> tok ParenOpen >> bexpr >> tok ParenClose >> tok Do >> stat >> peekTok >>= \t -> case t of
                                                                                     Break -> tok Break
                                                                                     _     -> return ()

bexpr :: CompilerStateT ()
bexpr = peekTok >>= \t -> case t of
        Conjunction   -> tok Conjunction >> bexpr >> bexpr
        Disjunction   -> tok Disjunction >> bexpr >> bexpr
        _ | isRelop t -> relop >> expr >> expr
        _             -> error ("unexpected token: " ++ show t)

expr :: CompilerStateT ()
expr = peekTok >>= \t -> case t of
       Plus         -> tok Plus >> operands
       Minus        -> tok Minus >> expr >> expr
       Multiply     -> tok Multiply >> operands
       Divide       -> tok Divide >> expr >> expr
       Number     _ -> tokNum
       Identifier _ -> tokId
       _            -> error ("unexpected token: " ++ show t)

operands :: CompilerStateT ()
operands = peekTok >>= \t -> case t of
           BracketOpen -> tok BracketOpen >> exprlist >> tok BracketClose
           _           -> expr >> expr

exprlist :: CompilerStateT ()
exprlist = expr >> peekTok >>= \t -> case t of
                   Comma -> tok Comma >> exprlist
                   _     -> return ()

relop :: CompilerStateT ()
relop = peekTok >>= \t -> case t of
        GreaterEqual -> tok GreaterEqual
        LessEqual    -> tok LessEqual
        Equal        -> tok Equal
        GreaterThan  -> tok GreaterThan
        LessThan     -> tok LessThan
        NotEqual     -> tok NotEqual
        _            -> error ("unexpected token: " ++ show t)
