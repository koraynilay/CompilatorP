module Lib.Instruction where

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

-- c = java class name
insToJ :: String -> Instruction -> String
insToJ _ Dup                 = " dup\n"
insToJ _ Pop                 = " pop\n"
insToJ _ Imul                = " imul\n"
insToJ _ Ineg                = " ineg\n"
insToJ _ Idiv                = " idiv\n"
insToJ _ Iadd                = " iadd\n"
insToJ _ Isub                = " isub\n"
insToJ _ Ior                 = " ior\n"
insToJ _ Iand                = " iand\n"
insToJ _ (Ldc val)           = " ldc " ++ show val ++ "\n"
insToJ _ (Istore addr)       = " istore " ++ show addr ++ "\n"
insToJ _ (Iload addr)        = " iload " ++ show addr ++ "\n"
insToJ _ (IfCmpEQ (Label l)) = " if_icmpeq " ++ l ++ "\n"
insToJ _ (IfCmpLE (Label l)) = " if_icmple " ++ l ++ "\n"
insToJ _ (IfCmpLT (Label l)) = " if_icmplt " ++ l ++ "\n"
insToJ _ (IfCmpNE (Label l)) = " if_icmpne " ++ l ++ "\n"
insToJ _ (IfCmpGE (Label l)) = " if_icmpge " ++ l ++ "\n"
insToJ _ (IfCmpGT (Label l)) = " if_icmpgt " ++ l ++ "\n"
insToJ _ (Ifne (Label l))    = " ifne " ++ l ++ "\n"
insToJ _ (Goto (Label l))    = " goto " ++ l ++ "\n"
insToJ c InvokePrint         = " invokestatic " ++ c ++ "/print(I)V\n"
insToJ c InvokeRead          = " invokestatic " ++ c ++ "/read()I\n"
