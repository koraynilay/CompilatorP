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

parse :: [Token] -> [Either Instruction Label]
parse ts = case execStateT prog $ initialState { tokens = ts } of
                (Just s) -> instructions s
                Nothing -> []

parseDebug :: [Token] -> Maybe ((), CompilerState)
parseDebug ts = runStateT prog $ initialState { tokens = ts }

nextVarAddrM :: CompilerStateT Int
nextVarAddrM = do
  s <- get
  let addr = nextVarAddr s
  put s { nextVarAddr = addr + 1 }
  return addr

getVarAddr :: String -> CompilerStateT (Maybe Int)
getVarAddr varName = do
  s <- get
  let t = symbolTable s
  return $ M.lookup varName t

getOrAddVarAddr :: String -> CompilerStateT Int
getOrAddVarAddr varName = do
  maybeAddr <- getVarAddr varName
  case maybeAddr of
    Just addr -> return addr
    Nothing -> do
      newAddr <- nextVarAddrM
      s <- get
      let t = symbolTable s
      put s { symbolTable = M.insert varName newAddr t }
      return newAddr

newLabel :: CompilerStateT Label
newLabel = do
  s <- get
  let labelNumber = labelCounter s
  put $ s { labelCounter = labelNumber + 1 }
  return $ Label $ "L" ++ show labelNumber

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

-- TODO: use ins : instructions s and reverse at the end for efficiency
emit :: Instruction -> CompilerStateT ()
emit ins = modify $ \s -> s { instructions = instructions s ++ [Left ins] }
emitL :: Label -> CompilerStateT ()
emitL ins = modify $ \s -> s { instructions = instructions s ++ [Right ins] }

-- prods

prog :: CompilerStateT ()
prog = (statlist >> tok EOF >> return ())

statlist :: CompilerStateT ()
statlist = stat >> ((tok Semicolon >> statlist) <|> return ())

stat :: CompilerStateT ()
stat = (do (Identifier var) <- tokId
           varAddr <- getOrAddVarAddr var
           tok Assignment
           assignv
           emit (Istore varAddr))
   <|> (tok Print >> tok BracketOpen >> exprlist >> tok BracketClose >> emit InvokePrint)
   <|> (do tok While
           tok ParenOpen
           startL <- newLabel
           jumpTo <- newLabel
           emitL startL
           bexpr jumpTo
           tok ParenClose
           tok Do
           stat
           emit (Goto startL)
           emitL jumpTo)
   <|> (do tok Conditional
           tok BracketOpen
           defaultL <- newLabel
           caselist defaultL
           tok BracketClose
           tok Default
           emitL defaultL
           stat)
   <|> (tok CurlyOpen >> statlist >> tok CurlyClose >> return ())

assignv :: CompilerStateT ()
assignv = (tok UserInput >> emit InvokeRead)
      <|> expr

caselist :: Label -> CompilerStateT ()
caselist defaultL = caseitem defaultL >> (caselist defaultL <|> return ())

caseitem :: Label -> CompilerStateT ()
caseitem defaultL = do tok Case
                       tok ParenOpen
                       jumpTo <- newLabel
                       bexpr jumpTo
                       tok ParenClose
                       tok Do
                       stat
                       caseitemd defaultL
                       emitL jumpTo

caseitemd :: Label -> CompilerStateT ()
caseitemd defaultL = (tok Break >> emit (Goto defaultL))
                 <|> return ()

bexpr :: Label -> CompilerStateT ()
bexpr jumpto = do jmpInstr <- relop
                  expr
                  expr
                  emit (jmpInstr jumpto)
--           <|> do tok Conjunction
--                  bexpr jumpto jumpto_or
--                  bexpr jumpto jumpto_or
--           <|> do tok Disjunction
--                  bexpr jumpto_or jumpto_or
--                  bexpr jumpto    jumpto_or

expr :: CompilerStateT ()
expr = (tok Plus >> operands >> emit Iadd)
   <|> (tok Minus >> expr >> expr >> emit Isub)
   <|> (tok Multiply >> operands >> emit Imul)
   <|> (tok Divide >> expr >> expr >> emit Idiv)
   <|> (do (Number n) <- tokNum
           emit (Ldc n))
   <|> (do (Identifier var) <- tokId
           maybeVarAddr <- getVarAddr var
           case maybeVarAddr of
             Just varAddr -> emit (Iload varAddr)
             Nothing -> error ("variable " ++ var ++ " not declared"))

operands :: CompilerStateT ()
operands = (expr >> expr)
       <|> (tok BracketOpen >> exprlist >> tok BracketClose >> return ())

exprlist :: CompilerStateT ()
exprlist = expr >> ((tok Comma >> exprlist) <|> return ())

relop :: CompilerStateT (Label -> Instruction)
relop = (tok GreaterEqual >> return IfCmpLT)
    <|> (tok LessEqual    >> return IfCmpGT)
    <|> (tok Equal        >> return IfCmpNE)
    <|> (tok GreaterThan  >> return IfCmpLE)
    <|> (tok LessThan     >> return IfCmpGE)
    <|> (tok NotEqual     >> return IfCmpEQ)
