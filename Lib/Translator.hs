module Lib.Translator where

import Lib.Token
import Lib.Lexer
import Lib.Instruction
import Lib.CodeGen

import qualified Data.Map as M
import Control.Monad.State
import Control.Applicative

type CompilerStateT = StateT CompilerState Maybe

data CompilerState = CompilerState
  { labelCounter :: Int
  , tokens       :: [Token]
  , instructions :: Code
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

parse :: [Token] -> Code
parse ts = case execStateT prog $ initialState { tokens = ts } of
                (Just s) -> reverse $ instructions s
                Nothing -> []

parseDebug :: [Token] -> Maybe ((), CompilerState)
parseDebug ts = runStateT prog $ initialState { tokens = ts }

nextVarAddrM :: CompilerStateT Int
nextVarAddrM = do
  addr <- gets nextVarAddr
  modify $ \s -> s { nextVarAddr = addr + 1 }
  return addr

getVarAddr :: String -> CompilerStateT (Maybe Int)
getVarAddr varName = gets (M.lookup varName . symbolTable)

getOrAddVarAddr :: String -> CompilerStateT Int
getOrAddVarAddr varName = do
  maybeAddr <- getVarAddr varName
  case maybeAddr of
    Just addr -> return addr
    Nothing -> do
      newAddr <- nextVarAddrM
      modify $ \s -> s { symbolTable = M.insert varName newAddr (symbolTable s) }
      return newAddr

newLabel :: CompilerStateT Label
newLabel = do
  labelNumber <- gets labelCounter
  modify $ \s -> s { labelCounter = labelNumber + 1 }
  return $ Label $ "L" ++ show labelNumber

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

emit :: Instruction -> CompilerStateT ()
emit ins = modify $ \s -> s { instructions = Left ins : instructions s }
emitL :: Label -> CompilerStateT ()
emitL ins = modify $ \s -> s { instructions = Right ins : instructions s }

-- prods

prog :: CompilerStateT ()
prog = (statlist >> tok EOF >> return ())

statlist :: CompilerStateT ()
statlist = stat >> peekTok >>= \t -> case t of
                   Semicolon -> tok Semicolon >> statlist
                   _         -> return ()

stat :: CompilerStateT ()
stat = peekTok >>= \t -> case t of
       Identifier var -> do tokId
                            varAddr <- getOrAddVarAddr var
                            tok Assignment
                            assignv
                            emit (Istore varAddr)
       Print          -> tok Print >> tok BracketOpen >> exprlist >> tok BracketClose >> emit InvokePrint
       While          -> do tok While
                            tok ParenOpen
                            startL <- newLabel
                            jumpTo <- newLabel
                            emitL startL
                            bexpr jumpTo
                            tok ParenClose
                            tok Do
                            stat
                            emit (Goto startL)
                            emitL jumpTo
       Conditional    -> do tok Conditional
                            tok BracketOpen
                            defaultL <- newLabel
                            caselist defaultL
                            tok BracketClose
                            tok Default
                            emitL defaultL
                            stat
       CurlyOpen      -> tok CurlyOpen >> statlist >> tok CurlyClose >> return ()
       _              -> empty

assignv :: CompilerStateT ()
assignv = peekTok >>= \t -> case t of
          UserInput -> tok UserInput >> emit InvokeRead
          _         -> expr

caselist :: Label -> CompilerStateT ()
caselist defaultL = caseitem defaultL >> peekTok >>= \t -> case t of
                                         Case -> caselist defaultL
                                         _    -> return ()

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
caseitemd defaultL = peekTok >>= \t -> case t of
                     Break -> tok Break >> emit (Goto defaultL)
                     _     -> return ()

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
expr = peekTok >>= \t -> case t of
       Plus           -> tok Plus >> operands >> emit Iadd
       Minus          -> tok Minus >> expr >> expr >> emit Isub
       Multiply       -> tok Multiply >> operands >> emit Imul
       Divide         -> tok Divide >> expr >> expr >> emit Idiv
       Number n       -> do tokNum
                            emit (Ldc n)
       Identifier var -> do tokId
                            maybeVarAddr <- getVarAddr var
                            case maybeVarAddr of
                              Just varAddr -> emit (Iload varAddr)
                              Nothing -> error ("variable " ++ var ++ " not declared")
       _              -> empty

operands :: CompilerStateT ()
operands = peekTok >>= \t -> case t of
           BracketOpen -> tok BracketOpen >> exprlist >> tok BracketClose >> return ()
           _           -> expr >> expr

exprlist :: CompilerStateT ()
exprlist = expr >> peekTok >>= \t -> case t of
                   Comma -> tok Comma >> exprlist
                   _     -> return ()

relop :: CompilerStateT (Label -> Instruction)
relop = peekTok >>= \t -> case t of
        GreaterEqual -> tok GreaterEqual >> return IfCmpLT
        LessEqual    -> tok LessEqual    >> return IfCmpGT
        Equal        -> tok Equal        >> return IfCmpNE
        GreaterThan  -> tok GreaterThan  >> return IfCmpLE
        LessThan     -> tok LessThan     >> return IfCmpGE
        NotEqual     -> tok NotEqual     >> return IfCmpEQ
        _            -> empty
