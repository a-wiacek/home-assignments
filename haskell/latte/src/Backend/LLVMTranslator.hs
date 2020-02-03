{-# LANGUAGE LambdaCase #-}
module Backend.LLVMTranslator(execLlvmTranslate) where
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Reader
import Data.Int
import qualified Data.Map.Strict as Map
import Text.Printf

import AbsLatte
import Backend.ClassDataLayout
import Backend.LLVMData
import Frontend.ClassHierarchy(buildClassHierarchy, funTypeToLlvmSignature)
import Frontend.FunctionSignatures

data TranslateEnv = TranslateEnv
    { funSignatures :: Map.Map String LlvmFunSignature
    , classesDataLayout :: ClassesDataLayout
    , currentClassLayout :: ClassDataLayout
    , currentClassName :: String
    , currentReturnType :: LlvmType
    }
initEnv :: FunctionSignatures -> ClassesDataLayout -> TranslateEnv
initEnv signs layout = TranslateEnv
    { funSignatures = Map.mapWithKey funTypeToLlvmSignature signs
    , classesDataLayout = layout
    , currentClassLayout = error "current class layout called outside of class definition"
    , currentClassName = error "current class name called outside of class definition"
    , currentReturnType = error "current return type called outside of function/method definition"
    }

data TranslateState = TranslateState
    { freshNameCounter :: Int
    , currentBlockIdent :: LlvmIdent
    , currentBlockInstrs :: [LlvmInstr]
    , producedBlocks :: [LlvmBlock]
    , identTypes :: Map.Map LlvmValue LlvmType
    , llvmPointers :: Map.Map String (LlvmValue, LlvmType) -- variable name |-> pointer + type of variable
    , llvmFunctions :: [LlvmFunction]
    , stringConstants :: Map.Map String Int
    }
initState :: TranslateState
initState = TranslateState
    { freshNameCounter = 0
    , currentBlockIdent = entryIdent
    , currentBlockInstrs = []
    , producedBlocks = []
    , identTypes = Map.empty
    , llvmPointers = Map.empty
    , llvmFunctions = []
    , stringConstants = Map.empty
    }

type LlvmMonad = ReaderT TranslateEnv (State TranslateState)

resetState :: LlvmMonad ()
resetState = modify $ \s -> initState
    { llvmFunctions = llvmFunctions s
    , stringConstants = stringConstants s
    }

setPtrs :: Map.Map String (LlvmValue, LlvmType) -> LlvmMonad ()
setPtrs ptrs = modify $ \s -> s { llvmPointers = ptrs }
addPtr :: String -> (LlvmValue, LlvmType) -> LlvmMonad ()
addPtr var info = modify $ \s -> s { llvmPointers = Map.insert var info (llvmPointers s)}

addRegType :: LlvmValue -> LlvmType -> LlvmMonad ()
addRegType value t = modify $ \s -> s { identTypes = Map.insert value t (identTypes s) }

emitAssignment :: LlvmExp -> LlvmMonad LlvmValue
emitAssignment exp = do
    i1 <- getFreshIdent
    emitInstr $ LlvmAssign i1 exp
    return $ LlvmValueIdent i1

emitInstr :: LlvmInstr -> LlvmMonad ()
emitInstr instr = do
    hasJump <- gets $ \case {h:_ -> isJumpInstr h; _ -> False} . currentBlockInstrs
    unless hasJump $ do
        modify (\s -> s { currentBlockInstrs = instr : currentBlockInstrs s })
        maybe (return ()) (uncurry (addRegType . LlvmValueIdent)) (findAssignment instr)

inClass :: String -> LlvmMonad a -> LlvmMonad a
inClass className f = do
    thisLayout <- asks ((Map.! className) . classesDataLayout)
    local (\env -> env { currentClassLayout = thisLayout, currentClassName = className }) f

getFreshIdent :: LlvmMonad LlvmIdent
getFreshIdent = do
    c <- gets freshNameCounter
    modify $ \s -> s { freshNameCounter = freshNameCounter s + 1 }
    return $ LlvmIdent (printf ".%d" c) False

getTypeSize :: LlvmType -> LlvmMonad Int32
getTypeSize = \case
    LlvmInt -> return 4
    LlvmString -> return 8
    LlvmBool -> return 1
    LlvmPtr _ -> return 8
    LlvmStaticArr _ -> error "called size on static array"
    LlvmRuntimeArr _ -> return 16
    LlvmVoid -> error "called size on void"
    LlvmClass className -> do
        fields <- asks (Map.elems . fieldsLayout . (Map.! className) . classesDataLayout)
        fieldsSize <- sum <$> forM fields (getTypeSize . snd)
        return $ fieldsSize + 8 -- 8 is for virtual table

checkValueType :: LlvmValue -> LlvmMonad LlvmType
checkValueType val = case val of
    LlvmValueIConst _ -> return LlvmInt
    LlvmValueBConst _ -> return LlvmBool
    LlvmValueIdent ident
         -> Map.lookup val <$> gets identTypes
        >>= maybe (error $ "No information about type of " ++ show ident) return
    _ -> error $ "No information about type of " ++ show val

produceDefaultValue :: LlvmType -> LlvmMonad LlvmValue
produceDefaultValue = \case
    LlvmInt -> return $ LlvmValueIConst 0
    LlvmBool -> return $ LlvmValueBConst False
    LlvmString -> translateExp $ EConstant n $ CString n ""
    LlvmPtr (LlvmClass className) -- this null "hiding" solves some type level problems
        -> emitAssignment $ LlvmBitcast LlvmString LlvmNull (LlvmPtr $ LlvmClass className)
    LlvmRuntimeArr t -> createArray t $ EConstant n $ CInteger n 0
    t -> error $ "Calling default value for unexpected type " ++ show t
    where n = Nothing

promoteToArg :: LlvmValue -> LlvmMonad LlvmArg
promoteToArg value = LlvmArg <$> checkValueType value <*> return value

getFunSignature :: Ident -> LlvmMonad LlvmFunSignature
getFunSignature ident
      = Map.lookup (unIdent ident) <$> asks funSignatures
    >>= maybe (error "no function signature in state") return

getPtrWithType :: String -> LlvmMonad (LlvmValue, LlvmType)
getPtrWithType name
      = Map.lookup name <$> gets llvmPointers
    >>= maybe (getClassFieldPtrWithType selfValue name) return

getClassFieldPtrWithType :: LlvmValue -> String -> LlvmMonad (LlvmValue, LlvmType)
getClassFieldPtrWithType classValue name = do
    (pos, fieldType) <- asks ((Map.! name) . fieldsLayout . currentClassLayout)
    classT <- asks (LlvmClass . currentClassName)
    val1 <- emitAssignment $ LlvmGetElementPtr (LlvmPtr fieldType) classT classValue
                             [(LlvmInt, LlvmValueIConst 0), (LlvmInt, LlvmValueIConst $ fromIntegral pos)]
    return (val1, LlvmPtr fieldType)

loadMethodFromObj :: LlvmValue -> Ident -> LlvmMonad (LlvmValue, LlvmType)
loadMethodFromObj classValue (Ident methodName) = do
    classT <- LlvmClass <$> asks currentClassName
    (pos, t, _) <- asks ((Map.! methodName) . virtualTable . currentClassLayout)
    -- get pointer of virtual table
    val1 <- emitAssignment $ LlvmGetElementPtr (LlvmPtr LlvmUnknownFunType) classT classValue
                             [(LlvmInt, LlvmValueIConst 0), (LlvmInt, LlvmValueIConst 0)]
    -- load virtual table
    val2 <- emitAssignment $ LlvmLoad (LlvmPtr $ LlvmPtr LlvmUnknownFunType) val1
    -- get pointer of the method
    val3 <- emitAssignment $ LlvmGetElementPtr (LlvmPtr LlvmUnknownFunType) (LlvmPtr LlvmUnknownFunType)
                             val2 [(LlvmInt, LlvmValueIConst $ fromIntegral pos)]
    val4 <- emitAssignment $ LlvmLoad (LlvmPtr LlvmUnknownFunType) val3
    val5 <- emitAssignment $ LlvmBitcast (LlvmPtr LlvmUnknownFunType) val4 (LlvmPtr t)
    return (val5, t)

loadMethodFromPtr :: LlvmValue -> Ident -> LlvmMonad (LlvmValue, LlvmType)
loadMethodFromPtr classPtr methodIdent = do
    classT <- asks (LlvmClass . currentClassName)
    val1 <- emitAssignment $ LlvmLoad (LlvmPtr classT) classPtr
    loadMethodFromObj val1 methodIdent

startNewBlock :: LlvmIdent -> LlvmInstr -> LlvmMonad ()
startNewBlock newBlockIdent jumpInstr = do
    unless (isJumpInstr jumpInstr) $ error "terminating block without jump instruction"
    emitInstr jumpInstr
    state <- get
    let oldBlock = LlvmBlock (currentBlockIdent state) (reverse $ currentBlockInstrs state)
    put $ state { currentBlockIdent = newBlockIdent
                , currentBlockInstrs = []
                , producedBlocks = oldBlock : producedBlocks state
                }

mkDeclPtr :: (Int, Int) -> String -> LlvmIdent
mkDeclPtr (x, y) var = LlvmIdent (printf "ptr.%s.%d.%d" var x y) False

mkDeclAlloc :: ((Int, Int), String, Type Pos) -> LlvmInstr
mkDeclAlloc (pos, var, t) = LlvmAssign (mkDeclPtr pos var) (LlvmAlloca $ typeToLlvmType t)

collectDecl :: Decl Pos -> [((Int, Int), String, Type Pos)]
collectDecl decl@(VarDecl a t items) = map (\x -> (position decl, name x, t)) items

mkDeclAllocs :: Decl Pos -> [LlvmInstr]
mkDeclAllocs = map mkDeclAlloc . collectDecl

correctType :: LlvmType -> LlvmValue -> LlvmMonad LlvmValue
correctType expectedType value = checkValueType value >>= \thisType -> if thisType == expectedType
    then return value
    else case (thisType, expectedType) of
        (LlvmRuntimeArr thisElType, LlvmRuntimeArr expectedElType) -> do
            -- LLVM forbids casting aggregate data types
            val1 <- emitAssignment $ LlvmExtractValue LlvmInt thisType value 0
            val2 <- emitAssignment $ LlvmExtractValue (LlvmPtr $ LlvmPtr thisElType) thisType value 1
            val3 <- emitAssignment $ LlvmBitcast (LlvmPtr thisElType) val2 (LlvmPtr expectedElType)
            val4 <- emitAssignment $ LlvmInsertValue expectedType LlvmUndef LlvmInt val1 0
            emitAssignment $ LlvmInsertValue expectedType val4 (LlvmPtr expectedElType) val3 1
        _ -> emitAssignment $ LlvmBitcast thisType value expectedType

mkArgPtr :: Arg Pos -> LlvmIdent
mkArgPtr arg = mkDeclPtr (position arg) (name arg)

mkArgAlloc :: Arg Pos -> LlvmInstr
mkArgAlloc arg = LlvmAssign (mkArgPtr arg) (LlvmAlloca $ typeToLlvmType $ argType arg)

mkPtrs :: [Arg Pos] -> Map.Map String (LlvmValue, LlvmType)
mkPtrs funArgs = Map.fromList [(name arg, (LlvmValueIdent $ mkArgPtr arg, typeToLlvmType $ argType arg)) | arg <- funArgs]

-----------------------------------------------------------------

translateFun :: FunDef Pos -> LlvmMonad [LlvmBlock]
translateFun def@(FunDef _ retType _ funArgs funBlock) = do
    forM_ (map mkArgAlloc funArgs) emitInstr
    forM_ funArgs $ \arg -> getFreshIdent >>= \ident ->
        emitInstr (LlvmStore (typeToLlvmType (argType arg))
                             (LlvmValueIdent ident)
                             (LlvmValueIdent $ mkArgPtr arg))
    setPtrs (mkPtrs funArgs)
    -- When transforming code to phi form, I need assumption that entry block
    -- has no loads. Therefore I force new block.
    realCodeIdent <- getFreshIdent
    startNewBlock realCodeIdent $ LlvmBr realCodeIdent
    local (\env -> env { currentReturnType = typeToLlvmType retType }) (translateBlock funBlock)
    finalBlockInstrs <- gets currentBlockInstrs
    currLabel <- gets currentBlockIdent
    let addFinalBlock = (LlvmBlock currLabel (reverse finalBlockInstrs) :)
    gets (reverse . addFinalBlock . producedBlocks)

translateTopFun :: FunDef Pos -> LlvmMonad ()
translateTopFun def@(FunDef _ retType _ funArgs _) = do
    resetState
    blocks <- translateFun def
    let llvmFunction = LlvmFunction sign blocks
    modify $ \s -> s { llvmFunctions = llvmFunction : llvmFunctions s }
    where sign = LlvmFunSignature
            (LlvmIdent (name def) True)
            (map (typeToLlvmType . argType) funArgs)
            (typeToLlvmType retType)

translateClassMember :: ClassMember Pos -> LlvmMonad ()
translateClassMember ClassFieldDef{} = return ()
translateClassMember (ClassMethodDef _ def@(FunDef _ retType _ funArgs _)) = do
    resetState
    void getFreshIdent
    className <- asks currentClassName
    let selfType = LlvmPtr $ LlvmClass className
    modify $ \s -> s { identTypes = Map.singleton selfValue selfType }
    blocks <- translateFun def
    let sign = LlvmFunSignature
            (LlvmIdent (printf "method.%s.%s" className $ name def) True)
            (LlvmPtr (LlvmClass className) : map (typeToLlvmType . argType) funArgs)
            (typeToLlvmType retType)
    let llvmFunction = LlvmFunction sign blocks
    modify $ \s -> s { llvmFunctions = llvmFunction : llvmFunctions s }


translateBlock :: Block Pos -> LlvmMonad ()
translateBlock block = do
    ptrs <- gets llvmPointers
    mapM_ translateStmt $ blockStmts block
    setPtrs ptrs

translateStmt :: Stmt Pos -> LlvmMonad ()
translateStmt = \case
    SEmpty _ -> return ()
    SBlock _ block -> translateBlock block
    SDecl _ decl@(VarDecl _ t items) -> do
        forM_ (mkDeclAllocs decl) emitInstr
        mapM_ (translateDeclItem (typeToLlvmType t) (position decl)) items
    SAssign _ lval exp -> do
        expVal <- translateExp exp
        translateLVal lval >>= maybe
            (error "lval has no value (in assignment)")
            (\val -> case val of
                LlvmValueIdent ident -> checkValueType val >>= \case
                    LlvmPtr t -> do
                        expVal' <- correctType t expVal
                        emitInstr $ LlvmStore t expVal' val
                    _ -> error "lvalue is not a pointer (in assignment)"
                _ -> error "lvalue expressed as a constant (in assignment)"
            )
    SIncr _ lval -> desugarXcrement OpPlus lval
    SDecr _ lval -> desugarXcrement OpMinus lval
    SRet _ exp -> do t <- asks currentReturnType
                     case t of -- This might look weird and redundant but optimizations need that to create better code
                        LlvmBool -> do  -- return x; => if (x) return true; else return false;
                            expVal <- translateExp exp
                            retTrueIdent <- getFreshIdent
                            retFalseIdent <- getFreshIdent
                            startNewBlock retTrueIdent $ LlvmBrIf expVal retTrueIdent retFalseIdent
                            startNewBlock retFalseIdent $ LlvmRet $ Just (t, LlvmValueBConst True)
                            emitInstr $ LlvmRet $ Just (t, LlvmValueBConst False)
                        _ -> do expVal <- translateExp exp >>= correctType t
                                emitInstr $ LlvmRet $ Just (t, expVal)
    SRetVoid _ -> emitInstr (LlvmRet Nothing)
    SIf _ exp stmt -> do
        val <- translateExp exp
        ifBodyIdent <- getFreshIdent
        postIfIdent <- getFreshIdent
        startNewBlock ifBodyIdent $ LlvmBrIf val ifBodyIdent postIfIdent
        translateStmt stmt
        startNewBlock postIfIdent $ LlvmBr postIfIdent
    SIfte _ exp trueStmt falseStmt -> replicateM 3 getFreshIdent >>= \case
        [trueBranchIdent, falseBranchIdent, postIfteIdent] -> do
        val <- translateExp exp
        startNewBlock trueBranchIdent $ LlvmBrIf val trueBranchIdent falseBranchIdent
        translateStmt trueStmt
        startNewBlock falseBranchIdent $ LlvmBr postIfteIdent
        translateStmt falseStmt
        startNewBlock postIfteIdent $ LlvmBr postIfteIdent
    SWhile _ exp stmt -> replicateM 3 getFreshIdent >>= \case
        [whileCondIdent, whileBodyIdent, postWhileIdent] -> do
        startNewBlock whileCondIdent $ LlvmBr whileCondIdent
        val <- translateExp exp
        startNewBlock whileBodyIdent $ LlvmBrIf val whileBodyIdent postWhileIdent
        translateStmt stmt
        startNewBlock postWhileIdent $ LlvmBr whileCondIdent
    for@SFor{} -> error "for was desugared"
    SExp _ exp -> void (translateExp exp)
    where desugarXcrement op lval = translateStmt $ SAssign n lval exp
            where n = Nothing
                  exp = EAdd n (ELVal n lval) (op n) (EConstant n $ CInteger n 1)
          translateDeclItem :: LlvmType -> (Int, Int) -> DeclItem Pos -> LlvmMonad ()
          translateDeclItem t pos decl = do
            expVal <- getExpVal
            addPtr (unIdent ident) (LlvmValueIdent $ mkDeclPtr pos $ unIdent ident, t)
            translateLVal (LValVar Nothing ident) >>= maybe
                (error "Value of void type appears in declaration")
                (\val -> case val of
                    LlvmValueIdent ident -> checkValueType val >>= \case
                        LlvmPtr t -> do
                            expVal' <- correctType t expVal
                            emitInstr $ LlvmStore t expVal' val
                        _ -> error "lvalue is not a pointer (in assignment)"
                    _ -> error "lvalue expressed as a constant (in assignment)"
                )
            where (getExpVal, ident) = case decl of
                    DeclNoInit _ i -> (produceDefaultValue t, i)
                    DeclWithInit _ i exp -> (translateExp exp, i)

callFunction :: LlvmValue -> [LlvmArg] -> LlvmType -> LlvmMonad (Maybe LlvmValue)
callFunction funIdent args = \case
    LlvmVoid -> emitAssignment (LlvmCall LlvmVoid funIdent args) >> return Nothing
    t -> Just <$> emitAssignment (LlvmCall t funIdent args)

translateLVal :: LVal Pos -> LlvmMonad (Maybe LlvmValue)
translateLVal = \case
    LValSelf _ -> return $ Just selfValue
    LValVar _ ident -> Just . fst <$> getPtrWithType (unIdent ident)
    LValAttr _ lval ident -> translateLVal' lval $ \val -> checkValueType val >>= \case
        LlvmPtr (LlvmRuntimeArr t) -> do -- it is length for sure
            arrVal <- emitAssignment $ LlvmLoad (LlvmRuntimeArr t) val
            fmap Just $ emitAssignment $ LlvmExtractValue LlvmInt (LlvmRuntimeArr t) arrVal 0
            -- This is perhaps a little bit longer than
            -- fmap Just $ emitAssignment $ LlvmGetElementPtr (LlvmPtr LlvmInt) (LlvmRuntimeArr t) val
            --                              [(LlvmInt, LlvmValueIConst 0), (LlvmInt, LlvmValueIConst 0)]
            -- but it is much easier to reduce this to phi form (no additional machinery is required)
        LlvmPtr (LlvmPtr (LlvmClass className)) -> inClass className $ do
            val1 <- emitAssignment $ LlvmLoad (LlvmPtr (LlvmClass className)) val
            Just . fst <$> getClassFieldPtrWithType val1 (unIdent ident)
        t -> error $ "calling attribute of a simple type " ++ show t
    LValCall _ (FunCall _ ident exps) -> do
        sign <- getFunSignature ident
        args <- mapM (\(exp, t) -> LlvmArg t <$> (translateExp exp >>= correctType t))
                     (zip exps $ llvmFunArgs sign)
        callFunction (LlvmValueIdent $ llvmFunName sign) args (llvmFunRetType sign)
    LValMethod _ lval (FunCall _ ident exps) -> translateLVal' lval $ \val -> checkValueType val >>= \case
        LlvmPtr (LlvmPtr (LlvmClass className)) -> do
            (methodIdent, methodType) <- inClass className $ loadMethodFromPtr val ident
            case methodType of
                LlvmFunType retType argTypes -> do
                    selfVal <- emitAssignment $ LlvmLoad (LlvmPtr (LlvmClass className)) val
                    expVals <- mapM translateExp exps
                    args <- mapM (\(expVal, t) -> LlvmArg t <$> correctType t expVal)
                                    (zip (selfVal : expVals) argTypes)
                    callFunction methodIdent args retType
                t -> error $ "method has non-functional type" ++ show t
        LlvmPtr (LlvmClass className) -> do
            (methodIdent, methodType) <- inClass className $ loadMethodFromObj val ident
            case methodType of
                LlvmFunType retType argTypes -> do
                    expVals <- mapM translateExp exps
                    args <- mapM (\(expVal, t) -> LlvmArg t <$> correctType t expVal)
                                    (zip (val : expVals) argTypes)
                    callFunction methodIdent args retType
                t -> error $ "method has non-functional type" ++ show t
        t -> error $ "calling method of not-a-class " ++ show t
    LValArrAccess _ lval exp -> translateLVal' lval $ \val -> checkValueType val >>= \case
        LlvmPtr (LlvmRuntimeArr t) ->  do
            index <- translateExp exp
            arrStruct <- emitAssignment $ LlvmLoad (LlvmRuntimeArr t) val
            rawArr <- emitAssignment $ LlvmExtractValue (LlvmPtr t) (LlvmRuntimeArr t) arrStruct 1
            fmap Just $ emitAssignment $ LlvmGetElementPtr (LlvmPtr t) t rawArr [(LlvmInt, index)]
        _ -> error "lvalue evaluated to not an array"
    where translateLVal' lval f = translateLVal lval >>= maybe (error "lvalue evaluated to nothing") f

createArray :: LlvmType -> Exp Pos -> LlvmMonad LlvmValue
createArray elemT exp = do
    elemTSize <- getTypeSize elemT
    expVal <- translateExp exp
    val1 <- emitAssignment $ LlvmCall LlvmString mallocIdent [LlvmArg LlvmInt expVal, LlvmArg LlvmInt $ LlvmValueIConst elemTSize]
    val2 <- emitAssignment $ LlvmBitcast LlvmString val1 (LlvmPtr elemT)
    val3 <- emitAssignment $ LlvmInsertValue (LlvmRuntimeArr elemT) LlvmUndef LlvmInt expVal 0
    emitAssignment $ LlvmInsertValue (LlvmRuntimeArr elemT) val3 (LlvmPtr elemT) val2 1

translateExp :: Exp Pos -> LlvmMonad LlvmValue
translateExp = \case
    ENullObj _ (Ident className) -> produceDefaultValue $ LlvmPtr $ LlvmClass className
    ELVal _ lval -> translateLVal lval >>= maybe
        -- this is intentionally hidden behind return, it should be ignored in void (translateExp exp)
        (return $ error "not ignoring value of type void")
        (\val -> case val of
            LlvmValueIdent ident -> checkValueType val >>= \t -> if isBasicType t
                then return val
                else emitAssignment $ LlvmLoad (derefType t) val
            _ -> error "lvalue expressed as a constant"
        )
    ENewObj _ (Ident className)
        -> emitAssignment $ LlvmCall (LlvmPtr $ LlvmClass className) (constructorValue className) []
    ENewArr _ t exp -> createArray (typeToLlvmType t) exp
    EConstant _ const -> case const of
        CInteger _ n -> return $ LlvmValueIConst (fromInteger n)
        CString _ str -> do
            constants <- gets stringConstants
            let size = Map.size constants
            maybe (modify (\s -> s { stringConstants = Map.insert str size constants }) >> loadStr size)
                  loadStr (Map.lookup str constants)
            where loadStr = emitAssignment . LlvmLoad LlvmString . LlvmValueIdent . mkStrIdent
        CTrue _ -> return $ LlvmValueBConst True
        CFalse _ -> return $ LlvmValueBConst False
    ENegate _ _ exp -> translateExp exp >>= \val -> case val of
        LlvmValueIConst n -> return $ LlvmValueIConst $ negate n
        LlvmValueBConst b -> return $ LlvmValueBConst $ not b
        LlvmValueIdent i1 -> checkValueType val >>= \case
            LlvmInt -> emitAssignment $ LlvmSub (LlvmValueIConst 0) val
            LlvmBool -> emitAssignment $ LlvmNeg val
    EMod _ exp1 op exp2 -> do
        (val1, val2) <- runSubexps exp1 exp2
        let constructor = case op of OpTimes _ -> LlvmMul
                                     OpDiv _ -> LlvmDiv
                                     OpMod _ -> LlvmMod
        emitAssignment $ constructor val1 val2
    EAdd _ exp1 op exp2 -> do
        (val1, val2) <- runSubexps exp1 exp2
        checkValueType val1 >>= \case
            LlvmString -> emitAssignment $ LlvmCall LlvmString concatStringsIdent
                                           [LlvmArg LlvmString val1, LlvmArg LlvmString val2]
            LlvmInt -> let constructor = case op of { OpPlus _ -> LlvmAdd; OpMinus _ -> LlvmSub }
                       in emitAssignment $ constructor val1 val2
    EComp _ exp1 op exp2 -> do
        (val1, val2) <- runSubexps exp1 exp2
        checkValueType val1 >>= \case
            LlvmString -> 
                let emitCall = emitAssignment $ LlvmCall LlvmString concatStringsIdent
                                                [LlvmArg LlvmString val1, LlvmArg LlvmString val2]
                in case op of OpEqual _ -> emitCall
                              OpNeq _ -> emitAssignment . LlvmNeg =<< emitCall
            t -> emitAssignment . LlvmCmp (opCompToLlvmCmpType op) t val1 =<< correctType t val2
    EAnd _ exp1 exp2 -> do
        val1 <- translateExp exp1
        currBlockIdent <- gets currentBlockIdent
        exp2BlockInitLabel <- getFreshIdent
        postAndIdent <- getFreshIdent
        startNewBlock exp2BlockInitLabel $ LlvmBrIf val1 exp2BlockInitLabel postAndIdent
        val2 <- translateExp exp2
        exp2BlockFinalLabel <- gets currentBlockIdent
        startNewBlock postAndIdent $ LlvmBr postAndIdent
        emitAssignment $ LlvmPhi LlvmBool [(LlvmValueBConst False, currBlockIdent), (val2, exp2BlockFinalLabel)]
    EOr _ exp1 exp2 -> do
        val1 <- translateExp exp1
        currBlockIdent <- gets currentBlockIdent
        exp2BlockInitLabel <- getFreshIdent
        postOrIdent <- getFreshIdent
        startNewBlock exp2BlockInitLabel $ LlvmBrIf val1 postOrIdent exp2BlockInitLabel
        val2 <- translateExp exp2
        exp2BlockFinalLabel <- gets currentBlockIdent
        startNewBlock postOrIdent $ LlvmBr postOrIdent
        emitAssignment $ LlvmPhi LlvmBool [(LlvmValueBConst True, currBlockIdent), (val2, exp2BlockFinalLabel)]
    where runSubexps exp1 exp2 = (,) <$> translateExp exp1 <*> translateExp exp2

-- define void @constructor.B(%struct.B* %.0)
translateClassConstructor :: String -> LlvmMonad ()
translateClassConstructor className = do
    resetState
    let classT = LlvmClass className
    objSize <- getTypeSize classT
    rawPtr <- emitAssignment $ LlvmCall LlvmString mallocIdent $ map (LlvmArg LlvmInt . LlvmValueIConst) [1, objSize]
    classPtr <- emitAssignment $ LlvmBitcast LlvmString rawPtr (LlvmPtr classT)
    let vtableT = LlvmPtr $ LlvmPtr LlvmUnknownFunType
    vtableSize <- asks (Map.size . virtualTable . currentClassLayout)
    vtable <- emitAssignment $ LlvmBitcast (LlvmPtr $ LlvmVTableStaticArr vtableSize) (vtableValue className) vtableT
    vtablePtr <- emitAssignment $ LlvmGetElementPtr vtableT classT classPtr
                                  [(LlvmInt, LlvmValueIConst 0), (LlvmInt, LlvmValueIConst 0)]
    emitInstr $ LlvmStore vtableT vtable vtablePtr
    fields <- asks (Map.elems . fieldsLayout . currentClassLayout)
    forM_ fields $ \(pos, t) -> do
        val <- produceDefaultValue t
        ptrIdent <- emitAssignment $ LlvmGetElementPtr (LlvmPtr t) classT classPtr
                                     [(LlvmInt, LlvmValueIConst 0), (LlvmInt, LlvmValueIConst $ fromIntegral pos)]
        emitInstr $ LlvmStore t val ptrIdent
    emitInstr $ LlvmRet (Just (LlvmPtr classT, classPtr))
    block <- LlvmBlock <$> gets currentBlockIdent <*> gets (reverse . currentBlockInstrs)
    let sign = LlvmFunSignature (constructorIdent className) [] (LlvmPtr classT)
    let llvmFunction = LlvmFunction sign [block]
    modify $ \s -> s { llvmFunctions = llvmFunction : llvmFunctions s }

translateClassDef :: String -> [ClassMember Pos] -> LlvmMonad ()
translateClassDef className members = inClass className $ do
    translateClassConstructor className
    forM_ members translateClassMember

translateTopDef :: TopDef Pos -> LlvmMonad ()
translateTopDef (TopFunDef _ funDef) = translateTopFun funDef
translateTopDef (BaseClassDef _ (Ident className) members) = translateClassDef className members
translateTopDef (ExtClassDef _ (Ident className) _ members) = translateClassDef className members

translateProg :: Program Pos -> LlvmMonad LlvmProgram
translateProg (Program _ defs) = do
    llvmClassesData <- asks (mkLlvmClassesData . classesDataLayout)
    let (vtables, typeDecls) = unzip $ Map.elems llvmClassesData
    mapM_ translateTopDef defs
    LlvmProgram typeDecls vtables
        <$> gets (map (uncurry LlvmStringConstant) . Map.assocs . stringConstants)
        <*> gets llvmFunctions

execLlvmTranslate :: FunctionSignatures -> ClassesDataLayout -> Program Pos -> LlvmProgram
execLlvmTranslate signs layout program = evalState
    (runReaderT (translateProg program) $ initEnv signs layout)
    initState
    