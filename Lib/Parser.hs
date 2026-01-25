module Lib.Parser where

import Lib.Token
import Lib.Lexer
import Lib.Instruction

import qualified Data.Map as M
import Control.Monad.State
import Control.Applicative

type CompilerStateT = StateT CompilerState Maybe

data CompilerState = CompilerState { tokens :: [Token] }
  deriving (Show)

initialState :: CompilerState
initialState = CompilerState { tokens = [] }

parse :: [Token] -> Bool
parse ts = case evalStateT prog $ initialState { tokens = ts } of
                Just x -> True
                Nothing -> False

parseDebug :: [Token] -> Maybe ((), CompilerState)
parseDebug ts = runStateT prog $ initialState { tokens = ts }

peekTok :: CompilerStateT Token
peekTok = do
  toks <- gets tokens
  case toks of
    (t:_) -> return t
    [] -> empty

getTok :: CompilerStateT Token
getTok = do
  toks <- gets tokens
  case toks of
    (t:ts) -> do
      modify $ \s -> s { tokens = ts }
      return t
    [] -> empty

sat :: (Token -> Bool) -> CompilerStateT ()
sat p = do
  t <- getTok
  if p t then return () else empty

tok :: Token -> CompilerStateT ()
tok t = sat (== t)

tokId :: CompilerStateT ()
tokId = do
  t <- getTok
  case t of
    Identifier var -> return ()
    _ -> empty

tokNum :: CompilerStateT ()
tokNum = do
  t <- getTok
  case t of
    Number n -> return ()
    _ -> empty

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
       _            -> empty

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
        _             -> empty

expr :: CompilerStateT ()
expr = peekTok >>= \t -> case t of
       Plus         -> tok Plus >> operands
       Minus        -> tok Minus >> expr >> expr
       Multiply     -> tok Multiply >> operands
       Divide       -> tok Divide >> expr >> expr
       Number     _ -> tokNum
       Identifier _ -> tokId
       _            -> empty

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
        _            -> empty
