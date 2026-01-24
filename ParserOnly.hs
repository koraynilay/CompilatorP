module ParserOnly where

import Token
import Lexer
import Instruction

import qualified Data.Map as M
import Control.Monad.State
import Control.Applicative

type CompilerStateT = StateT CompilerState Maybe

data CompilerState = CompilerState
  { labelCounter :: Int
  , tokens       :: [Token]
  , instructions :: [Either Instruction Label]
  , symbolTable  :: M.Map String Int
  , nextVarAddr :: Int
  } deriving (Show)

initialState :: CompilerState
initialState = CompilerState
  { labelCounter = 0
  , tokens       = []
  , instructions = []
  , symbolTable  = M.empty
  , nextVarAddr = 0
  }

parse :: [Token] -> Bool
parse ts = case evalStateT prog $ initialState { tokens = ts } of
                Just x -> True
                Nothing -> False

parseDebug :: [Token] -> Maybe ((), CompilerState)
parseDebug ts = runStateT prog $ initialState { tokens = ts }

nextVarAddrM :: CompilerStateT Int
nextVarAddrM = do
  s <- get
  let addr = nextVarAddr s
  put s { nextVarAddr = addr + 1 }
  return addr

-- TODO: handle variable not declared
getVarAddr :: String -> CompilerStateT Int
getVarAddr varName = do
  s <- get
  let t = symbolTable s
  case M.lookup varName t of
    Just addr -> return addr
    Nothing -> do
      newAddr <- nextVarAddrM
      put s { symbolTable = M.insert varName newAddr t }
      return newAddr

newLabel :: CompilerStateT Label
newLabel = do
  s <- get
  let labelNumber = labelCounter s
  put $ s { labelCounter = labelNumber + 1 }
  return $ Label $ "L" ++ show labelNumber ++ ":"

getTok :: CompilerStateT Token
getTok = do
  s <- get
  case tokens s of
    (t:ts) -> do
      put (s { tokens = ts })
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
