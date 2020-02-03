module Backend.LLVMData where
import Data.Char(ord)
import Data.Int
import Data.List(intercalate, isPrefixOf)
import qualified Data.Map.Strict as Map
import Text.Printf

import AbsLatte

data LlvmIdent = LlvmIdent
    { unllvmIdent :: String
    , llvmGlobal :: Bool
    } deriving (Eq, Ord)
instance Show LlvmIdent where
    show (LlvmIdent ident global) = (if global then '@' else '%') : ident

entryIdent = LlvmIdent "entry" False
concatStringsIdent = LlvmValueIdent $ LlvmIdent "internal.concatStrings" True
compareStringsIdent = LlvmValueIdent $ LlvmIdent "internal.compareStrings" True
mallocIdent = LlvmValueIdent $ LlvmIdent "internal.malloc" True
selfValue = LlvmValueIdent $ LlvmIdent ".0" False  -- self ident is always %.0, since it is the first argument
constructorIdent className = LlvmIdent ("constructor." ++ className) True
constructorValue = LlvmValueIdent . constructorIdent
vtableValue className = LlvmValueIdent $ LlvmIdent ("vtable." ++ className) True
isPtr value = case value of
    LlvmValueIdent ident -> "ptr" `isPrefixOf` unllvmIdent ident
    _ -> False

data LlvmType
    = LlvmInt -- i32
    | LlvmString -- i8*, also used as equivalent of C void* (when using malloc)
    | LlvmBool -- i1
    | LlvmPtr { derefType :: LlvmType }
    | LlvmStaticArr Int
    | LlvmVTableStaticArr Int
    | LlvmRuntimeArr LlvmType
    | LlvmVoid
    | LlvmClass String -- pointer to class object of type %struct.classname
    | LlvmFunType LlvmType [LlvmType]
    | LlvmUnknownFunType
    deriving (Eq, Ord)

instance Show LlvmType where
    show LlvmInt = "i32"
    show LlvmString = "i8*"
    show LlvmBool = "i1"
    show (LlvmPtr t) = show t ++ "*"
    show (LlvmVTableStaticArr n) = printf "[%d x void (...)*]" n
    show (LlvmStaticArr n) = printf "[%d x i8]" (n + 1) -- +1 is for '\0' at the end of the string
    show (LlvmRuntimeArr t) = printf "{ i32, %s* }" (show t)
    show LlvmVoid = "void"
    show (LlvmClass s) = printf "%%struct.%s" s
    show (LlvmFunType retType argTypes) = printf "%s (%s)" (show retType) (showMany argTypes)
    show LlvmUnknownFunType = "void (...)"

typeToLlvmType :: Type a -> LlvmType
typeToLlvmType (IntType _) = LlvmInt
typeToLlvmType (StringType _) = LlvmString
typeToLlvmType (BoolType _) = LlvmBool
typeToLlvmType (VoidType _) = LlvmVoid
typeToLlvmType (ClassType _ s) = LlvmPtr $ LlvmClass $ unIdent s
typeToLlvmType (ArrayType _ t) = LlvmRuntimeArr (typeToLlvmType t)

-- Type is basic ~ does not require dereferencing
isBasicType :: LlvmType -> Bool
isBasicType (LlvmPtr (LlvmClass _)) = True -- !
isBasicType (LlvmPtr _) = False
isBasicType _ = True


data LlvmGlobalTypeDecl = LlvmGlobalTypeDecl String [LlvmType] deriving (Eq, Ord)

instance Show LlvmGlobalTypeDecl where
    show (LlvmGlobalTypeDecl className types) = printf "%%struct.%s = type { void (...)**%s }"
        className (concatMap ((", "++) . show) types)


-- String is class name: table name is "@vtable.B" for class B
data LlvmVTable = LlvmVTable String [(LlvmType, LlvmIdent)] deriving (Eq, Ord)

instance Show LlvmVTable where
    show (LlvmVTable className functions) = printf "@vtable.%s = global [%d x void (...)*] %s"
        className (length functions) $ if null functions
            then "[]"
            else let f (t, ident) = printf "void (...)* bitcast (%s* %s to void (...)*)" (show t) (show ident)
                 in printf "[\n  %s\n]" (intercalate ",\n  " $ map f functions)


data LlvmValue
    = LlvmValueIConst Int32
    | LlvmValueBConst Bool
    | LlvmValueIdent LlvmIdent
    | LlvmUndef
    | LlvmNull
    deriving (Eq, Ord)

instance Show LlvmValue where
    show (LlvmValueIConst n) = show n
    show (LlvmValueBConst b) = if b then "1" else "0"
    show (LlvmValueIdent ident) = show ident
    show LlvmUndef = "undef"
    show LlvmNull = "null"


data LlvmArg = LlvmArg
    { llvmArgType :: LlvmType
    , llvmArgName :: LlvmValue
    } deriving (Eq, Ord)

instance Show LlvmArg where
    show (LlvmArg t v) = show t ++ " " ++ show v

showMany :: Show a => [a] -> String
showMany = intercalate ", " . map show

data LlvmProgram = LlvmProgram 
    { globalTypeDecls :: [LlvmGlobalTypeDecl]
    , virtualTables :: [LlvmVTable]
    , llvmStringConstants :: [LlvmStringConstant]
    , functions :: [LlvmFunction]
    } deriving (Eq, Ord)

instance Show LlvmProgram where
    show (LlvmProgram typeDecls vtables constants functions) = unlines
         $ llvmPrelude
        ++ map show typeDecls
        ++ map show vtables
        ++ map show constants
        ++ map show functions
        where llvmPrelude =
                [ "declare void @printInt(i32)"
                , "declare void @printString(i8*)"
                , "declare void @error()"
                , "declare i32 @readInt()"
                , "declare i8* @readString()"
                , "declare i8* @internal.concatStrings(i8*, i8*)"
                , "declare i1 @internal.compareStrings(i8*, i8*)"
                , "declare i8* @internal.malloc(i32, i32)"
                ]

data LlvmFunSignature = LlvmFunSignature
    { llvmFunName :: LlvmIdent
    , llvmFunArgs :: [LlvmType] -- names of arguments are %.0, %.1, ...
    , llvmFunRetType :: LlvmType
    } deriving (Eq, Ord)

instance Show LlvmFunSignature where
    show (LlvmFunSignature name args retType) = printf "%s %s(%s)"
        (show retType)
        (show name)
        (showMany $ zipWith f args [0..])
        where f t v = LlvmArg t $ LlvmValueIdent $ LlvmIdent ('.' : show v) False


data LlvmStringConstant = LlvmStringConstant String Int deriving (Eq, Ord)

mkStrIdent, mkStaticStrIdent :: Int -> LlvmIdent
mkStrIdent n = LlvmIdent ("str." ++ show n) True
mkStaticStrIdent n = LlvmIdent ("staticstr." ++ show n) True

instance Show LlvmStringConstant where
    show (LlvmStringConstant s n) = printf "%s = constant [%d x i8] %s\n%s = global i8* bitcast ([%d x i8]* %s to i8*)"
        staticIdent size stringRep ident size staticIdent
        where size = length s + 1
              ident = show (mkStrIdent n)
              staticIdent = show (mkStaticStrIdent n)
              stringRep = "[" ++ concatMap (printf "i8 %d, " . ord) s ++ "i8 0]"

data LlvmFunction = LlvmFunction
    { funSign :: LlvmFunSignature
    , funBlocks :: [LlvmBlock]
    } deriving (Eq, Ord)

instance Show LlvmFunction where
    show (LlvmFunction sign blocks) = unlines $
        ("define " ++ show sign ++ " {") : (map show blocks ++ ["}"])

funToBlockMap :: LlvmFunction -> Map.Map LlvmIdent LlvmBlock
funToBlockMap (LlvmFunction _ blocks) = Map.fromList [(blockLabel b, b) | b <- blocks]
blockMapToBlocks :: Map.Map LlvmIdent LlvmBlock -> [LlvmBlock]
blockMapToBlocks blocks = blocks Map.! entryIdent : Map.elems (Map.delete entryIdent blocks)

data LlvmBlock = LlvmBlock
    { blockLabel :: LlvmIdent
    , blockInstructions :: [LlvmInstr]
    } deriving (Eq, Ord)

instance Show LlvmBlock where
    show (LlvmBlock label instrs) = unlines $
        (unllvmIdent label ++ ":") : map (("  "++) . show) instrs

blockJumpInstr :: LlvmBlock -> LlvmInstr
blockJumpInstr = last . blockInstructions

computeSuccessors :: LlvmBlock -> [LlvmIdent]
computeSuccessors (LlvmBlock _ instrs) = case instrs of
    [] -> []
    l -> case last l of LlvmRet _ -> []
                        LlvmBr ident -> [ident]
                        LlvmBrIf _ ident1 ident2 -> [ident1, ident2]

modifyLastInstr :: (LlvmInstr -> LlvmInstr) -> LlvmBlock -> LlvmBlock
modifyLastInstr f block =
    let instrs = blockInstructions block
    in block { blockInstructions = init instrs ++ [f $ last instrs] }

data LlvmCmpType
    = CmpEq
    | CmpNeq
    | CmpSgt
    | CmpSge
    | CmpSlt
    | CmpSle
    deriving (Eq, Ord)

instance Show LlvmCmpType where
    show CmpEq = "eq"
    show CmpNeq = "ne"
    show CmpSgt = "sgt"
    show CmpSge = "sge"
    show CmpSlt = "slt"
    show CmpSle = "sle"

opCompToLlvmCmpType :: OpComp a -> LlvmCmpType
opCompToLlvmCmpType (OpLt _) = CmpSlt
opCompToLlvmCmpType (OpLeq _) = CmpSle
opCompToLlvmCmpType (OpGt _) = CmpSgt
opCompToLlvmCmpType (OpGeq _) = CmpSge
opCompToLlvmCmpType (OpEqual _) = CmpEq
opCompToLlvmCmpType (OpNeq _) = CmpNeq

llvmCmpTypeToOp :: Ord a => LlvmCmpType -> a -> a -> Bool
llvmCmpTypeToOp CmpEq = (==)
llvmCmpTypeToOp CmpNeq = (/=)
llvmCmpTypeToOp CmpSgt = (>)
llvmCmpTypeToOp CmpSge = (>=)
llvmCmpTypeToOp CmpSlt = (<)
llvmCmpTypeToOp CmpSle = (<=)


data LlvmExp
    = LlvmNeg LlvmValue -- this is only for boolean negation (instruction produced is still sub,
                        -- this is just to make typechecking easier)
    | LlvmAdd LlvmValue LlvmValue
    | LlvmSub LlvmValue LlvmValue
    | LlvmMul LlvmValue LlvmValue
    | LlvmDiv LlvmValue LlvmValue
    | LlvmMod LlvmValue LlvmValue
    | LlvmShl LlvmValue Int
    | LlvmShr LlvmValue Int
    | LlvmAnd LlvmValue LlvmValue
    | LlvmAlloca LlvmType
    | LlvmLoad LlvmType -- type of value, pointer should have type T*
               LlvmValue -- pointer
    | LlvmCmp LlvmCmpType LlvmType LlvmValue LlvmValue
    | LlvmCall LlvmType LlvmValue [LlvmArg]
    | LlvmPhi LlvmType [(LlvmValue, LlvmIdent)]
    | LlvmGetElementPtr LlvmType -- type of result
                        LlvmType -- type of the bigger value
                        LlvmValue -- the bigger value
                        [(LlvmType, LlvmValue)] -- nested references
    | LlvmBitcast LlvmType -- source type
                  LlvmValue -- casted value
                  LlvmType -- target type
    | LlvmInsertValue LlvmType -- type of result
                      LlvmValue -- structure
                      LlvmType -- type of inserted value
                      LlvmValue -- inserted value
                      Int -- index of field
    | LlvmExtractValue LlvmType -- type of result
                       LlvmType -- aggregate type
                       LlvmValue -- aggregate value
                       Int -- index of field
    deriving (Eq, Ord)

instance Show LlvmExp where
    show (LlvmNeg val) = printf "sub i1 1, %s" (show val)
    show (LlvmAdd val1 val2) = printf "add i32 %s, %s" (show val1) (show val2)
    show (LlvmSub val1 val2) = printf "sub i32 %s, %s" (show val1) (show val2)
    show (LlvmMul val1 val2) = printf "mul i32 %s, %s" (show val1) (show val2)
    show (LlvmDiv val1 val2) = printf "sdiv i32 %s, %s" (show val1) (show val2)
    show (LlvmMod val1 val2) = printf "srem i32 %s, %s" (show val1) (show val2)
    show (LlvmShl val n) = printf "shl i32 %s, %d" (show val) n
    show (LlvmShr val n) = printf "ashr i32 %s, %d" (show val) n
    show (LlvmAnd val1 val2) = printf "and i32 %s, %s" (show val1) (show val2)
    show (LlvmAlloca t) = printf "alloca %s" (show t)
    show (LlvmLoad t val) = printf "load %s, %s* %s" (show t) (show t) (show val)
    show (LlvmCmp cmpT t val1 val2) = printf "icmp %s %s %s, %s" (show cmpT) (show t) (show val1) (show val2)
    show (LlvmCall t ident args) = printf "call %s %s(%s)" (show t) (show ident) (showMany args)
    show (LlvmPhi t vals) = printf "phi %s %s" (show t) $
        intercalate ", " $ map (\(val, label) -> printf "[%s, %s]" (show val) (show label)) vals
    show (LlvmGetElementPtr _ ptrT val nested) = printf "getelementptr %s, %s* %s%s"
        (show ptrT) (show ptrT) (show val) (concatMap f nested)
        where f :: (LlvmType, LlvmValue) -> String
              f (t, v) = printf ", %s %s" (show t) (show v)
    show (LlvmBitcast initT val finalT) = printf "bitcast %s %s to %s" (show initT) (show val) (show finalT)
    show (LlvmInsertValue finalT val insertT insertVal ix) = printf "insertvalue %s %s, %s %s, %d"
        (show finalT) (show val) (show insertT) (show insertVal) ix
    show (LlvmExtractValue _ aggT val ix) = printf "extractvalue %s %s, %d" (show aggT) (show val) ix

llvmExpType :: LlvmExp -> LlvmType
llvmExpType LlvmNeg{} = LlvmBool
llvmExpType LlvmAdd{} = LlvmInt
llvmExpType LlvmSub{} = LlvmInt
llvmExpType LlvmMul{} = LlvmInt
llvmExpType LlvmDiv{} = LlvmInt
llvmExpType LlvmMod{} = LlvmInt
llvmExpType LlvmShl{} = LlvmInt
llvmExpType LlvmShr{} = LlvmInt
llvmExpType LlvmAnd{} = LlvmInt
llvmExpType (LlvmAlloca t) = LlvmPtr t
llvmExpType (LlvmLoad t _) = t
llvmExpType LlvmCmp{} = LlvmBool
llvmExpType (LlvmCall t _ _) = t
llvmExpType (LlvmPhi t _) = t
llvmExpType (LlvmGetElementPtr t _ _ _) = t
llvmExpType (LlvmBitcast _ _ t) = t
llvmExpType (LlvmInsertValue t _ _ _ _) = t
llvmExpType (LlvmExtractValue t _ _ _) = t

isPureExp :: LlvmExp -> Bool
isPureExp LlvmAlloca{} = False
isPureExp (LlvmLoad _ val) = case val of
    LlvmValueIdent ident -> llvmGlobal ident
    _ -> False
isPureExp LlvmCall{} = False
isPureExp _ = True

data LlvmInstr
    = LlvmRet (Maybe (LlvmType, LlvmValue))
    | LlvmBr LlvmIdent
    | LlvmBrIf LlvmValue LlvmIdent LlvmIdent
    | LlvmStore LlvmType LlvmValue LlvmValue
    | LlvmAssign LlvmIdent LlvmExp
    deriving (Eq, Ord)

instance Show LlvmInstr where
    show (LlvmRet Nothing) = "ret void"
    show (LlvmRet (Just (t, val))) = printf "ret %s %s" (show t) (show val)
    show (LlvmBr label) = "br label " ++ show label
    show (LlvmBrIf val label1 label2) = printf "br i1 %s, label %s, label %s" (show val) (show label1) (show label2)
    show (LlvmStore t val ptr) = printf "store %s %s, %s* %s" (show t) (show val) (show t) (show ptr)
    show (LlvmAssign ident exp) = case llvmExpType exp of
        LlvmVoid -> show exp
        _ -> printf "%s = %s" (show ident) (show exp)

isJumpInstr :: LlvmInstr -> Bool
isJumpInstr LlvmBr{} = True
isJumpInstr LlvmBrIf{} = True
isJumpInstr LlvmRet{} = True
isJumpInstr _ = False

isPureInstr :: LlvmInstr -> Bool
isPureInstr (LlvmAssign _ exp) = isPureExp exp
isPureInstr _ = False

findAssignment :: LlvmInstr -> Maybe (LlvmIdent, LlvmType)
findAssignment (LlvmAssign ident exp) = case llvmExpType exp of
    LlvmVoid -> Nothing
    t -> Just (ident, t)
findAssignment _ = Nothing