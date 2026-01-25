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
statlist = stat >> ((tok Semicolon >> statlist) <|> return ())

stat :: CompilerStateT ()
stat = (tokId >> tok Assignment >> assignv)
   <|> (tok Print >> tok BracketOpen >> exprlist >> tok BracketClose)
   <|> (tok While >> tok ParenOpen >> bexpr >> tok ParenClose >> tok Do >> stat)
   <|> (tok Conditional >> tok BracketOpen >> caselist >> tok BracketClose >> tok Default >> stat)
   <|> (tok CurlyOpen >> statlist >> tok CurlyClose)

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

expr :: CompilerStateT ()
expr = (tok Plus >> operands)
   <|> (tok Minus >> expr >> expr)
   <|> (tok Multiply >> operands)
   <|> (tok Divide >> expr >> expr)
   <|> (tokNum)
   <|> (tokId)

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
