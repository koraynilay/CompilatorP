module Lib.TranslatorNotLL1 where

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
statlist = stat >> ((tok Semicolon >> statlist) <|> return ())

stat :: CompilerStateT ()
stat = (do (Identifier var) <- tokId
           varAddr <- getOrAddVarAddr var
           tok Assignment
           assignv
           emit (Istore varAddr))
   <|> (tok Print >> tok BracketOpen >> exprlist True >> tok BracketClose >> return ())
   <|> (do tok While
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
           emitL (jumpto jdata))
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
                       jdata <- jumpData
                       bexpr jdata
                       tok ParenClose
                       tok Do
                       emitL (jbody jdata)
                       stat
                       caseitemd defaultL
                       emitL (jumpto jdata)

caseitemd :: Label -> CompilerStateT ()
caseitemd defaultL = (tok Break >> emit (Goto defaultL))
                 <|> return ()

bexpr :: JumpData -> CompilerStateT ()
bexpr jdata = (do jmpInstr <- relop (notinv jdata)
                  expr
                  expr
                  let jloc = if notinv jdata then jbody jdata else jumpto jdata
                  emit (jmpInstr jloc))
          <|> (do tok Conjunction
                  bexpr jdata
                  bexpr jdata)
          <|> (do tok Disjunction
                  let jdata' = jdata { notinv = True }
                  bexpr jdata'
                  bexpr jdata')

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
       <|> (tok BracketOpen >> exprlist False >> tok BracketClose >> return ())

exprlist :: Bool -> CompilerStateT ()
exprlist print = expr >> when print (emit InvokePrint) >> ((tok Comma >> exprlist print) <|> return ())

relop :: Bool -> CompilerStateT (Label -> Instruction)
relop notinv = (tok GreaterEqual >> return (if notinv then IfCmpGE else IfCmpLT))
           <|> (tok LessEqual    >> return (if notinv then IfCmpLE else IfCmpGT))
           <|> (tok Equal        >> return (if notinv then IfCmpEQ else IfCmpNE))
           <|> (tok GreaterThan  >> return (if notinv then IfCmpGT else IfCmpLE))
           <|> (tok LessThan     >> return (if notinv then IfCmpLT else IfCmpGE))
           <|> (tok NotEqual     >> return (if notinv then IfCmpNE else IfCmpEQ))
