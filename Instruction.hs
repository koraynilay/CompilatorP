module Instruction where

data Label = Label String
  deriving (Eq)

instance Show Label where
  show :: Label -> String
  show (Label l) = l ++ ":\n"

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
                   deriving (Eq)

instance Show Instruction where
  show :: Instruction -> String
  show Dup                 = " dup\n"
  show Pop                 = " pop\n"
  show Imul                = " imul\n"
  show Ineg                = " ineg\n"
  show Idiv                = " idiv\n"
  show Iadd                = " iadd\n"
  show Isub                = " isub\n"
  show Ior                 = " ior\n"
  show Iand                = " iand\n"
  show (Ldc val)           = " ldc " ++ show val ++ "\n"
  show (Istore addr)       = " istore " ++ show addr ++ "\n"
  show (Iload addr)        = " iload " ++ show addr ++ "\n"
  show (IfCmpEQ (Label l)) = " if_icmpeq " ++ l ++ "\n"
  show (IfCmpLE (Label l)) = " if_icmple " ++ l ++ "\n"
  show (IfCmpLT (Label l)) = " if_icmplt " ++ l ++ "\n"
  show (IfCmpNE (Label l)) = " if_icmpne " ++ l ++ "\n"
  show (IfCmpGE (Label l)) = " if_icmpge " ++ l ++ "\n"
  show (IfCmpGT (Label l)) = " if_icmpgt " ++ l ++ "\n"
  show (Ifne (Label l))    = " ifne " ++ l ++ "\n"
  show (Goto (Label l))    = " goto " ++ l ++ "\n"
  show InvokePrint         = " invokestatic Output/print(I)V\n"
  show InvokeRead          = " invokestatic Output/read()I\n"
