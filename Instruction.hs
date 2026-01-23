module Instruction where

data Instruction = Ldc Int
                 | Imul
                 | Ineg
                 | Idiv
                 | Iadd
                 | Isub
                 | Istore Int
                 | Ior
                 | Iand
                 | Iload Int
                 | If_icmpeq Int
                 | If_icmple Int
                 | If_icmplt Int
                 | If_icmpne Int
                 | If_icmpge Int
                 | If_icmpgt Int
                 | Ifne Int
                 | Goto Int
                 | InvokeStatic Int  -- 1 per print, 0 (o altro) per read
                 | Dup
                 | Pop
                 | Label Int
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
toJasmin (If_icmpeq l)   = " if_icmpeq L" ++ show l ++ "\n"
toJasmin (If_icmple l)   = " if_icmple L" ++ show l ++ "\n"
toJasmin (If_icmplt l)   = " if_icmplt L" ++ show l ++ "\n"
toJasmin (If_icmpne l)   = " if_icmpne L" ++ show l ++ "\n"
toJasmin (If_icmpge l)   = " if_icmpge L" ++ show l ++ "\n"
toJasmin (If_icmpgt l)   = " if_icmpgt L" ++ show l ++ "\n"
toJasmin (Ifne l)        = " ifne L" ++ show l ++ "\n"
toJasmin (Goto l)        = " goto L" ++ show l ++ "\n"
toJasmin (InvokeStatic 1) = " invokestatic Output/print(I)V\n"
toJasmin (InvokeStatic _) = " invokestatic Output/read()I\n"
toJasmin (Label l)       = "L" ++ show l ++ ":\n"
