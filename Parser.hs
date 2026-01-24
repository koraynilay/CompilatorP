module Parser where

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
  , instructions :: [Instruction]
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

newLabel :: CompilerStateT String
newLabel = do
  s <- get
  let labelNumber = labelCounter s
  put $ s { labelCounter = labelNumber + 1 }
  return $ "L" ++ show labelNumber ++ ":"

getTok :: CompilerStateT Token
getTok = do
  s <- get
  let (t:ts) = tokens s
  put (s { tokens = ts })
  return t

sat :: (Token -> Bool) -> CompilerStateT Token
sat p = do
  t <- getTok
  if p t then return t else empty

tok :: CompilerStateT Token
tok = sat (== tok)

-- prods

prog :: CompilerStateT ()
prog = statlist >> tok EOF

statlist :: CompilerStateT ()
statlist = stat >> statlistp
       <|> tok Semicolon >> stat >> statlistp

statlistp :: CompilerStateT ()
statlistp = tok Semicolon >> stat >> statlistp
        <|> return

stat :: CompilerStateT ()
stat = do (Identifier var) <- tok Identifier
          tok Assignment
          assignv
          varAddr <- getVarAddr var
          emit (Istore varAddr)
   <|> tok Print >> tok BracketOpen >> exprlist >> tok BracketClose
   <|> tok While >> tok ParenOpen >> bexpr >> tok Do >> stat
   <|> tok Conditional >> tok BracketOpen >> caselist >> tok BracketClose >> tok Default >> stat
   <|> tok CurlyOpen >> statlist >> tok CurlyClose

assignv :: CompilerStateT ()
assignv = do tok UserInput
             emit InvokeRead
      <|> expr

caselist :: CompilerStateT ()
caselist = caseitem >> caselistp

caselistp :: CompilerStateT ()
caselistp = caseitem >> caselistp
        <|> return

caseitem :: CompilerStateT ()
caseitem = tok Case >> tok ParenOpen >> bexpr >> tok ParenClose >> tok Do >> stat >> caseitemd

caseitemd :: CompilerStateT ()
caseitemd = tok Break <|> return

bexpr :: CompilerStateT ()
bexpr = relop >> expr >> expr

expr :: CompilerStateT ()
expr = tok Plus >> operands
   <|> tok Minus >> expr >> expr
   <|> tok Multiply >> mult
   <|> tok Divide >> expr expr
   <|> do (Number n) <- tok Number
          emit (Ldc n)
   <|> do (Identifier var) <- tok Identifier
          varAddr <- getVarAddr var
          emit (Iload varAddr) -- TODO: handle variable not declared

operands :: CompilerStateT ()
operands = expr >> expr
       <|> tok BracketOpen >> exprlist >> tok BracketOpen

-- plus :: CompilerStateT ()
-- plus = expr >> expr
--    <|> tok BracketOpen >> exprlist >> tok BracketOpen
-- 
-- mult :: CompilerStateT ()
-- mult = expr >> expr
--    <|> tok BracketOpen >> exprlist >> tok BracketOpen

exprlist :: CompilerStateT ()
exprlist = expr >> exprlistp

exprlistp :: CompilerStateT ()
exprlistp = tok Comma >> expr >> exprlistp
        <|> return () -- () needed?

relop :: CompilerStateT ()
relop = tok GreaterEqual >> emit 





--  s <- get
--  case tokens s of
--    (Print:ts) -> expr >> getVarAddr var >>= \addr -> emit (Istore addr)
--    (Identifier var:Assignment:UserInput:ts) -> put (s { tokens = ts }) >> emit InvokeRead >> getVarAddr var >>= \addr -> emit (Istore addr)
--    (Identifier var:Assignment:ts) -> expr >> getVarAddr var >>= \addr -> emit (Istore addr)

  

emit :: Instruction -> CompilerStateT ()
-- TODO: use ins : instructions s and reverse at the end for efficiency
emit ins = modify $ \s -> s { instructions = instructions s ++ [ins] }


--prog :: [Token] -> [Instruction]
--prog xs = endProg $ statlist xs
--
--endProg :: [Token] -> [Instruction]
--endProg (TokenSimple EOF:_) = []
--
--statlist :: [Token] -> [Instruction]

