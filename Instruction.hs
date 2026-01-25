module Instruction where

newtype Label = Label String
  deriving (Show, Eq)

labToJ :: Label -> String
labToJ (Label l) = l ++ ":\n"

data Instruction = Dup
                 | Pop
                 | Imul
                 | Ineg
                 | Idiv
                 | Iadd
                 | Isub
                 | Ior
                 | Iand
                 | Ldc Integer -- to change in case we want Integral(s)
                 | Istore Int
                 | Iload Int
                 | IfCmpEQ Label
                 | IfCmpLE Label
                 | IfCmpLT Label
                 | IfCmpNE Label
                 | IfCmpGE Label
                 | IfCmpGT Label
                 | Ifne Label
                 | Goto Label
                 | InvokePrint
                 | InvokeRead
                   deriving (Show, Eq)

insToJ :: Instruction -> String
insToJ Dup                 = " dup\n"
insToJ Pop                 = " pop\n"
insToJ Imul                = " imul\n"
insToJ Ineg                = " ineg\n"
insToJ Idiv                = " idiv\n"
insToJ Iadd                = " iadd\n"
insToJ Isub                = " isub\n"
insToJ Ior                 = " ior\n"
insToJ Iand                = " iand\n"
insToJ (Ldc val)           = " ldc " ++ show val ++ "\n"
insToJ (Istore addr)       = " istore " ++ show addr ++ "\n"
insToJ (Iload addr)        = " iload " ++ show addr ++ "\n"
insToJ (IfCmpEQ (Label l)) = " if_icmpeq " ++ l ++ "\n"
insToJ (IfCmpLE (Label l)) = " if_icmple " ++ l ++ "\n"
insToJ (IfCmpLT (Label l)) = " if_icmplt " ++ l ++ "\n"
insToJ (IfCmpNE (Label l)) = " if_icmpne " ++ l ++ "\n"
insToJ (IfCmpGE (Label l)) = " if_icmpge " ++ l ++ "\n"
insToJ (IfCmpGT (Label l)) = " if_icmpgt " ++ l ++ "\n"
insToJ (Ifne (Label l))    = " ifne " ++ l ++ "\n"
insToJ (Goto (Label l))    = " goto " ++ l ++ "\n"
insToJ InvokePrint         = " invokestatic Output/print(I)V\n"
insToJ InvokeRead          = " invokestatic Output/read()I\n"
