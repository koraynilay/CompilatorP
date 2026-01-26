module Lib.Translator where

import Lib.Token
import Lib.Lexer
import Lib.Instruction
import Lib.CodeGen
import qualified Lib.CompilerState as CS
import Lib.CompilerState (CompilerStateT
                         , getTok , peekTok
                         , newLabel
                         , tok , tokId , tokNum
                         , emit, emitL
                         , getOrAddVarAddr , getVarAddr)

import Control.Applicative
import Control.Monad (when)

data JumpData = JumpData
  { jumpto :: Label
  , jbody :: Label
  , notinv :: Bool }

jumpData :: CompilerStateT JumpData
jumpData = do
  jumptoL <- newLabel
  jbodyL  <- newLabel
  return JumpData { jumpto = jumptoL, jbody = jbodyL, notinv = False }

parse :: [Token] -> Code
parse ts = case CS.parse prog ts of
                (Just ((),s)) -> reverse $ CS.instructions s
                Nothing -> []

parseDebug :: [Token] -> Maybe ((), CS.CompilerState)
parseDebug ts = CS.parse prog ts

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
       Print          -> tok Print >> tok BracketOpen >> exprlist InvokePrint >> tok BracketClose >> return ()
       While          -> do tok While
                            tok ParenOpen
                            startL <- newLabel
                            jdata <- jumpData
                            emitL startL
                            bexpr jdata
                            tok ParenClose
                            tok Do
                            emitL (jbody jdata)
                            stat
                            emit (Goto startL)
                            emitL (jumpto jdata)
       Conditional    -> do tok Conditional
                            tok BracketOpen
                            defaultL <- newLabel
                            caselist defaultL
                            tok BracketClose
                            tok Default
                            stat
                            emitL defaultL
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
                       jdata <- jumpData
                       bexpr jdata
                       tok ParenClose
                       tok Do
                       emitL (jbody jdata)
                       stat
                       caseitemd defaultL
                       emitL (jumpto jdata)

caseitemd :: Label -> CompilerStateT ()
caseitemd defaultL = peekTok >>= \t -> case t of
                     Break -> tok Break >> emit (Goto defaultL)
                     _     -> return ()

bexpr :: JumpData -> CompilerStateT ()
bexpr jdata = peekTok >>= \t -> case t of
              Conjunction   -> do tok Conjunction
                                  bexpr jdata
                                  bexpr jdata
              Disjunction   -> do tok Disjunction
                                  let jdata' = jdata { notinv = True }
                                  bexpr jdata'
                                  bexpr jdata'
              _ | isRelop t -> do jmpInstr <- relop (notinv jdata)
                                  expr
                                  expr
                                  let jloc = if notinv jdata then jbody jdata else jumpto jdata
                                  emit (jmpInstr jloc)
              _             -> empty


expr :: CompilerStateT ()
expr = peekTok >>= \t -> case t of
       Plus           -> tok Plus >> operands Iadd >> return ()
       Minus          -> tok Minus >> expr >> expr >> emit Isub
       Multiply       -> tok Multiply >> operands Imul >> return ()
       Divide         -> tok Divide >> expr >> expr >> emit Idiv
       Number n       -> do tokNum
                            emit (Ldc n)
       Identifier var -> do tokId
                            maybeVarAddr <- getVarAddr var
                            case maybeVarAddr of
                              Just varAddr -> emit (Iload varAddr)
                              Nothing -> error ("variable " ++ var ++ " not declared")
       _              -> empty

operands :: Instruction -> CompilerStateT ()
operands inst = peekTok >>= \t -> case t of
                BracketOpen -> tok BracketOpen >> exprlist inst >> tok BracketClose >> return ()
                _           -> expr >> expr >> emit inst

exprlist :: Instruction -> CompilerStateT ()
exprlist InvokePrint = expr >> emit InvokePrint >> peekTok >>= \t -> case t of
                                                   Comma -> tok Comma >> exprlist InvokePrint
                                                   _     -> return ()
exprlist inst = expr >> exprlistp inst

exprlistp :: Instruction -> CompilerStateT ()
exprlistp inst = peekTok >>= \t -> case t of
                 Comma -> tok Comma >> expr >> emit inst >> exprlistp inst
                 _     -> return ()

relop :: Bool -> CompilerStateT (Label -> Instruction)
relop notinv = peekTok >>= \t -> case t of
               GreaterEqual -> tok GreaterEqual >> return (if notinv then IfCmpGE else IfCmpLT)
               LessEqual    -> tok LessEqual    >> return (if notinv then IfCmpLE else IfCmpGT)
               Equal        -> tok Equal        >> return (if notinv then IfCmpEQ else IfCmpNE)
               GreaterThan  -> tok GreaterThan  >> return (if notinv then IfCmpGT else IfCmpLE)
               LessThan     -> tok LessThan     >> return (if notinv then IfCmpLT else IfCmpGE)
               NotEqual     -> tok NotEqual     >> return (if notinv then IfCmpNE else IfCmpEQ)
               _            -> empty
