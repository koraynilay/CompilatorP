module Instruction where

data Label = Label String
  deriving (Show, Eq)

labelToJasmin :: Label -> String
labelToJasmin (Label l) = l ++ ":\n"

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


-- Funzione toJasmin con pattern matching
toJasmin :: Instruction -> String
toJasmin Dup                 = " dup\n"
toJasmin Pop                 = " pop\n"
toJasmin Imul                = " imul\n"
toJasmin Ineg                = " ineg\n"
toJasmin Idiv                = " idiv\n"
toJasmin Iadd                = " iadd\n"
toJasmin Isub                = " isub\n"
toJasmin Ior                 = " ior\n"
toJasmin Iand                = " iand\n"
toJasmin (Ldc val)           = " ldc " ++ show val ++ "\n"
toJasmin (Istore addr)       = " istore " ++ show addr ++ "\n"
toJasmin (Iload addr)        = " iload " ++ show addr ++ "\n"
toJasmin (IfCmpEQ (Label l)) = " if_icmpeq " ++ l ++ "\n"
toJasmin (IfCmpLE (Label l)) = " if_icmple " ++ l ++ "\n"
toJasmin (IfCmpLT (Label l)) = " if_icmplt " ++ l ++ "\n"
toJasmin (IfCmpNE (Label l)) = " if_icmpne " ++ l ++ "\n"
toJasmin (IfCmpGE (Label l)) = " if_icmpge " ++ l ++ "\n"
toJasmin (IfCmpGT (Label l)) = " if_icmpgt " ++ l ++ "\n"
toJasmin (Ifne (Label l))    = " ifne " ++ l ++ "\n"
toJasmin (Goto (Label l))    = " goto " ++ l ++ "\n"
toJasmin InvokePrint         = " invokestatic Output/print(I)V\n"
toJasmin InvokeRead          = " invokestatic Output/read()I\n"
