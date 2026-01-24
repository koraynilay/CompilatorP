module Instruction where

data Instruction = Dup
                 | Pop
                 | Imul
                 | Ineg
                 | Idiv
                 | Iadd
                 | Isub
                 | Ior
                 | Iand
                 | Ldc Int
                 | Istore Int
                 | Iload Int
                 | IfCmpEQ Int
                 | IfCmpLE Int
                 | IfCmpLT Int
                 | IfCmpNE Int
                 | IfCmpGE Int
                 | IfCmpGT Int
                 | Ifne Int
                 | Goto Int
                 | InvokePrint
                 | InvokeRead
                 | Label String
                   deriving (Show, Eq)

-- Funzione toJasmin con pattern matching
toJasmin :: Instruction -> String
toJasmin Dup             = " dup\n"
toJasmin Pop             = " pop\n"
toJasmin Imul            = " imul\n"
toJasmin Ineg            = " ineg\n"
toJasmin Idiv            = " idiv\n"
toJasmin Iadd            = " iadd\n"
toJasmin Isub            = " isub\n"
toJasmin Ior             = " ior\n"
toJasmin Iand            = " iand\n"
toJasmin (Ldc val)       = " ldc " ++ show val ++ "\n"
toJasmin (Istore addr)   = " istore " ++ show addr ++ "\n"
toJasmin (Iload addr)    = " iload " ++ show addr ++ "\n"
toJasmin (IfCmpEQ l)     = " if_icmpeq L" ++ show l ++ "\n"
toJasmin (IfCmpLE l)     = " if_icmple L" ++ show l ++ "\n"
toJasmin (IfCmpLT l)     = " if_icmplt L" ++ show l ++ "\n"
toJasmin (IfCmpNE l)     = " if_icmpne L" ++ show l ++ "\n"
toJasmin (IfCmpGE l)     = " if_icmpge L" ++ show l ++ "\n"
toJasmin (IfCmpGT l)     = " if_icmpgt L" ++ show l ++ "\n"
toJasmin (Ifne l)        = " ifne L" ++ show l ++ "\n"
toJasmin (Goto l)        = " goto L" ++ show l ++ "\n"
toJasmin InvokePrint     = " invokestatic Output/print(I)V\n"
toJasmin InvokeRead      = " invokestatic Output/read()I\n"
toJasmin (Label l)       = l
