module Lib.CompilerState where

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
  , readTokens   :: [Token]
  , instructions :: Code
  , symbolTable  :: M.Map String Int
  , nextVarAddr  :: Int
  } deriving (Show)

initialState :: CompilerState
initialState = CompilerState
  { labelCounter = 0
  , tokens       = []
  , readTokens   = []
  , instructions = []
  , symbolTable  = M.empty
  , nextVarAddr = 0
  }

-- first grammar function -> tokens -> resulting state
parse :: CompilerStateT () -> [Token] -> Maybe ((), CompilerState)
parse f ts = runStateT f $ initialState { tokens = ts }

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

peekLastTok :: CompilerStateT (Maybe Token)
peekLastTok = do
  readToks <- gets readTokens
  case readToks of
    (t:_) -> return $ Just t
    [] -> return Nothing

getTok :: CompilerStateT Token
getTok = do
  toks <- gets tokens
  case toks of
    (t:ts) -> do
      modify $ \s -> s { tokens = ts
                       , readTokens = t : readTokens s }
      return t
    [] -> empty

ungetTok :: CompilerStateT Token
ungetTok = do
  readToks <- gets readTokens
  case readToks of
    (t:ts) -> do
      modify $ \s -> s { readTokens = ts
                       , tokens = t : tokens s }
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

aroundErrTok :: CompilerStateT String
aroundErrTok = peekLastTok >>= \maybeTok -> case maybeTok of
               Just before -> do unexpected <- getTok
                                 after      <- getTok
                                 return $ show before ++ show unexpected ++ show after
               Nothing     -> do unexpected <- getTok
                                 after      <- getTok
                                 afterafter <- getTok
                                 return $ show unexpected ++ show after ++ show afterafter
