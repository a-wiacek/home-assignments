module LLVMData where
import AbsInstant
import Data.Int
import Text.Printf

data LLVMCode = LLVMCode [LLVMInstr]
data LLVMMatOp = LLVMAdd | LLVMSub | LLVMMul | LLVMDiv
newtype LLVMIdent = LLVMIdent String
data LLVMValue = LLVMValueConst Int32 | LLVMValueIdent LLVMIdent
data LLVMInstr = LLVMAlloc LLVMIdent -- allocated pointer
               | LLVMStore LLVMValue -- stored value
                           LLVMIdent -- pointer
               | LLVMLoad  LLVMIdent -- new variable
                           LLVMIdent -- pointer
               | LLVMMatOp LLVMIdent -- result
                           LLVMMatOp
                           LLVMValue -- argument 1
                           LLVMValue -- argument 2
               | LLVMPrint LLVMValue


instance Show LLVMCode where
    show (LLVMCode c) = unlines $
        [ "declare void @printInt(i32)"
        , "define i32 @main() {"
        ] ++ map show c ++
        [ "  ret i32 0"
        , "}"
        ]

instance Show LLVMMatOp where
    show LLVMAdd = "add"
    show LLVMSub = "sub"
    show LLVMMul = "mul"
    show LLVMDiv = "sdiv"

instance Show LLVMIdent where
    show (LLVMIdent s) = printf "%%%s" s

instance Show LLVMValue where
    show (LLVMValueConst n) = show n
    show (LLVMValueIdent p) = show p

instance Show LLVMInstr where
    show (LLVMAlloc p)   = printf "  %s = alloca i32" (show p)
    show (LLVMStore v p) = printf "  store i32 %s, i32* %s" (show v) (show p)
    show (LLVMLoad v p)  = printf "  %s = load i32, i32* %s" (show v) (show p)
    show (LLVMMatOp r op arg1 arg2)
                         = printf "  %s = %s i32 %s, %s" (show r) (show op) (show arg1) (show arg2)
    show (LLVMPrint v)   = printf "  call void @printInt(i32 %s)" (show v)
