module JVMData where
import AbsInstant
import Data.Int
import qualified Data.Map.Strict as Map
import Text.Printf

data JVMCode = JVMCode
    { className :: String
    , jvmInstructions :: [JVMInstr]
    }

data JVMInstr = JVMConst Int32
              | JVMLoad Int
              | JVMStore Int
              | JVMAdd
              | JVMSub
              | JVMMul
              | JVMDiv
              | JVMSwap
              | JVMGetPrintStream
              | JVMPrint

getLocalsLimit :: JVMCode -> Int
getLocalsLimit = maximum . (1:) . map localCalled . jvmInstructions where
    localCalled :: JVMInstr -> Int
    localCalled (JVMLoad n) = n + 1
    localCalled (JVMStore n) = n + 1
    localCalled _ = 0

getStackLimit :: JVMCode -> Int
getStackLimit = maximum . scanl (+) 0 . map stackDiff . jvmInstructions where
    stackDiff :: JVMInstr -> Int
    stackDiff (JVMConst _) = 1
    stackDiff (JVMLoad _) = 1
    stackDiff JVMGetPrintStream = 1
    stackDiff JVMSwap = 0
    stackDiff _ = -1

instance Show JVMCode where
    show jvmCode = unlines $
        [ printf ".class public %s" $ className jvmCode
        , ".super java/lang/Object"
        , ""
        , ".method public <init>()V"
        , "  aload_0"
        , "  invokespecial java/lang/Object/<init>()V"
        , "  return"
        , ".end method"
        , ""
        , ".method public static main([Ljava/lang/String;)V"
        , printf "  .limit locals %d" $ getLocalsLimit jvmCode
        , printf "  .limit stack %d" $ getStackLimit jvmCode
        ] ++ map show (jvmInstructions jvmCode) ++
        [ "  return"
        , ".end method"
        ]

instance Show JVMInstr where
    show (JVMConst n)
        | n == -1 = "  iconst_m1"
        | n >= 0 && n <= 5 = printf "  iconst_%d" n
        | n >= -128 && n < 128 = printf "  bipush %d" n -- [-2^7, 2^7)
        | n >= -32768 && n < 32768 = printf "  sipush %d" n -- [-2^15, 2^15)
        | otherwise = printf "  ldc %d" n
    show (JVMLoad p)  = printf "  iload%c%s" (if p < 4 then '_' else ' ') (show p) 
    show (JVMStore p) = printf "  istore%c%s" (if p < 4 then '_' else ' ') (show p)
    show JVMGetPrintStream = "  getstatic java/lang/System/out Ljava/io/PrintStream;"
    show JVMAdd   = "  iadd"
    show JVMSub   = "  isub"
    show JVMMul   = "  imul"
    show JVMDiv   = "  idiv"
    show JVMSwap  = "  swap"
    show JVMPrint = "  invokevirtual java/io/PrintStream/println(I)V"
