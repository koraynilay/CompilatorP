module Instruction where

header :: String
header = unlines
  [ ".class public Output"
  , ".super java/lang/Object"
  , ""
  , ".method public <init>()V"
  , " aload_0"
  , " invokenonvirtual java/lang/Object/<init>()V"
  , " return"
  , ".end method"
  , ""
  , ".method public static print(I)V"
  , " .limit stack 2"
  , " getstatic java/lang/System/out Ljava/io/PrintStream;"
  , " iload_0"
  , " invokestatic java/lang/Integer/toString(I)Ljava/lang/String;"
  , " invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V"
  , " return"
  , ".end method"
  , ""
  , ".method public static read()I"
  , " .limit stack 3"
  , " new java/util/Scanner"
  , " dup"
  , " getstatic java/lang/System/in Ljava/io/InputStream;"
  , " invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V"
  , " invokevirtual java/util/Scanner/next()Ljava/lang/String;"
  , " invokestatic java/lang/Integer.parseInt(Ljava/lang/String;)I"
  , " ireturn"
  , ".end method"
  , ""
  , ".method public static run()V"
  , " .limit stack 1024"
  , " .limit locals 256"
  ]

footer :: String
footer = unlines
  [ " return"
  , ".end method"
  , ""
  , ".method public static main([Ljava/lang/String;)V"
  , " invokestatic Output/run()V"
  , " return"
  , ".end method"
  ]

toJasmin :: [Either Instruction Label] -> String
toJasmin xs = header ++ (foldl (++) "" $ map toJasminSingle xs) ++ footer

toJasminSingle :: Either Instruction Label -> String
toJasminSingle (Left  i) = instructionToJasmin i
toJasminSingle (Right l) = labelToJasmin l

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

instructionToJasmin :: Instruction -> String
instructionToJasmin Dup                 = " dup\n"
instructionToJasmin Pop                 = " pop\n"
instructionToJasmin Imul                = " imul\n"
instructionToJasmin Ineg                = " ineg\n"
instructionToJasmin Idiv                = " idiv\n"
instructionToJasmin Iadd                = " iadd\n"
instructionToJasmin Isub                = " isub\n"
instructionToJasmin Ior                 = " ior\n"
instructionToJasmin Iand                = " iand\n"
instructionToJasmin (Ldc val)           = " ldc " ++ show val ++ "\n"
instructionToJasmin (Istore addr)       = " istore " ++ show addr ++ "\n"
instructionToJasmin (Iload addr)        = " iload " ++ show addr ++ "\n"
instructionToJasmin (IfCmpEQ (Label l)) = " if_icmpeq " ++ l ++ "\n"
instructionToJasmin (IfCmpLE (Label l)) = " if_icmple " ++ l ++ "\n"
instructionToJasmin (IfCmpLT (Label l)) = " if_icmplt " ++ l ++ "\n"
instructionToJasmin (IfCmpNE (Label l)) = " if_icmpne " ++ l ++ "\n"
instructionToJasmin (IfCmpGE (Label l)) = " if_icmpge " ++ l ++ "\n"
instructionToJasmin (IfCmpGT (Label l)) = " if_icmpgt " ++ l ++ "\n"
instructionToJasmin (Ifne (Label l))    = " ifne " ++ l ++ "\n"
instructionToJasmin (Goto (Label l))    = " goto " ++ l ++ "\n"
instructionToJasmin InvokePrint         = " invokestatic Output/print(I)V\n"
instructionToJasmin InvokeRead          = " invokestatic Output/read()I\n"
