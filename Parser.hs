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

sat :: (Token -> Bool) -> CompilerStateT Token
sat p = do
  t <- getTok
  if p t then return t else empty

tok :: Token -> CompilerStateT Token
tok t = sat (== t)

tokId :: CompilerStateT Token
tokId = do
  t <- getTok
  case t of
    Identifier var -> return t
    _ -> empty

tokNum :: CompilerStateT Token
tokNum = do
  t <- getTok
  case t of
    Number n -> return t
    _ -> empty

-- prods

prog :: CompilerStateT ()
prog = do l <- newLabel
          statlist l
          emitL l
          tok EOF
          return ()

statlist :: Label -> CompilerStateT ()
statlist nextL = do snl <- newLabel
                    stat snl
                    emitL snl
                    statlistp nextL

statlistp :: Label -> CompilerStateT ()
statlistp nextL = do tok Semicolon 
                     snl <- newLabel
                     stat snl
                     emitL snl
                     statlistp nextL
              <|> emit (Goto nextL)

stat :: Label -> CompilerStateT ()
stat nextL = do (Identifier var) <- tokId
                tok Assignment
                assignv
                varAddr <- getVarAddr var
                emit (Istore varAddr)
         <|> tok Print >> tok BracketOpen >> exprlist >> tok BracketClose >> emit InvokePrint >> emit (Goto nextL)
         <|> do tok While
                tok ParenOpen
                bexprTrue <- newLabel
                bexpr bexprTrue nextL
                tok ParenClose
                tok Do
                emitL bexprTrue
                stat nextL
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

-- label true -> label false -> state
bexpr :: Label -> Label -> CompilerStateT ()
bexpr = do relop
           expr
           expr
           emit

expr :: CompilerStateT ()
expr = tok Plus >> operands
   <|> tok Minus >> expr >> expr
   <|> tok Multiply >> operands
   <|> tok Divide >> expr expr
   <|> do (Number n) <- tokNum
          emit (Ldc n)
   <|> do (Identifier var) <- tokId
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

relop :: CompilerStateT Instruction
relop = tok GreaterEqual >> 





-- TODO: use ins : instructions s and reverse at the end for efficiency
emit :: Instruction -> CompilerStateT ()
emit ins = modify $ \s -> s { instructions = instructions s ++ [Left ins] }
emitL :: Label -> CompilerStateT ()
emitL ins = modify $ \s -> s { instructions = instructions s ++ [Right ins] }



--  s <- get
--  case tokens s of
--    (Print:ts) -> expr >> getVarAddr var >>= \addr -> emit (Istore addr)
--    (Identifier var:Assignment:UserInput:ts) -> put (s { tokens = ts }) >> emit InvokeRead >> getVarAddr var >>= \addr -> emit (Istore addr)
--    (Identifier var:Assignment:ts) -> expr >> getVarAddr var >>= \addr -> emit (Istore addr)

  


--prog :: [Token] -> [Instruction]
--prog xs = endProg $ statlist xs
--
--endProg :: [Token] -> [Instruction]
--endProg (TokenSimple EOF:_) = []
--
--statlist :: [Token] -> [Instruction]

