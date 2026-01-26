module Lib.Translator where

import Lib.Token
import Lib.Lexer
import Lib.Instruction
import Lib.CodeGen
import qualified Lib.CompilerState as CS
import Lib.CompilerState (CompilerStateT, aroundErrTok
                         , getTok , peekTok
                         , newLabel
                         , tok , tokId , tokNum
                         , emit, emitL
                         , getOrAddVarAddr , getVarAddr)

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
       Print          -> tok Print >> tok BracketOpen >> exprlist >> tok BracketClose >> emit InvokePrint
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
                            emitL defaultL
                            stat
       CurlyOpen      -> tok CurlyOpen >> statlist >> tok CurlyClose >> return ()
       _              -> aroundErrTok >>= \ar -> error ("unexpected token: " ++ show t ++ " around " ++ ar)

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
              _             -> aroundErrTok >>= \ar -> error ("unexpected token: " ++ show t ++ " around " ++ ar)


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
       _              -> aroundErrTok >>= \ar -> error ("unexpected token: " ++ show t ++ " around " ++ ar)

operands :: CompilerStateT ()
operands = peekTok >>= \t -> case t of
           BracketOpen -> tok BracketOpen >> exprlist >> tok BracketClose >> return ()
           _           -> expr >> expr

exprlist :: CompilerStateT ()
exprlist = expr >> peekTok >>= \t -> case t of
                   Comma -> tok Comma >> exprlist
                   _     -> return ()

relop :: Bool -> CompilerStateT (Label -> Instruction)
relop notinv = peekTok >>= \t -> case t of
               GreaterEqual -> tok GreaterEqual >> return (if notinv then IfCmpGE else IfCmpLT)
               LessEqual    -> tok LessEqual    >> return (if notinv then IfCmpLE else IfCmpGT)
               Equal        -> tok Equal        >> return (if notinv then IfCmpEQ else IfCmpNE)
               GreaterThan  -> tok GreaterThan  >> return (if notinv then IfCmpGT else IfCmpLE)
               LessThan     -> tok LessThan     >> return (if notinv then IfCmpLT else IfCmpGE)
               NotEqual     -> tok NotEqual     >> return (if notinv then IfCmpNE else IfCmpEQ)
               _            -> aroundErrTok >>= \ar -> error ("unexpected token: " ++ show t ++ " around " ++ ar)
