{-# LANGUAGE LambdaCase #-}
module Frontend.Typecheck(typecheck) where
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map

import AbsLatte
import Frontend.ClassHierarchy
import Frontend.ClassAnalysis
import Frontend.Environment
import Frontend.FunctionSignatures

data TypeEnv = TypeEnv
    { variables :: Map.Map String Int
    , functions :: Map.Map String (FunType Pos)
    , fields :: Map.Map String (Type Pos)
    , methods :: Map.Map String (Map.Map String (FunType Pos)) -- className -> methodName -> type
    , expectedReturnType :: Type Pos
    , selfType :: Maybe String
    -- Keep position of first declaration in block to check that variables in block have unique names
    , thisBlockVars :: Map.Map String (Int, Int)
    , errTrace :: [String]
    , hierarchy :: ClassHierarchy
    , classesInfo :: ClassesInfo
    }

instance Environment TypeEnv where
    updateTrace tr env = env { errTrace = tr : errTrace env }
    dropTrace env = env { errTrace = tail (errTrace env) }
    getTrace = errTrace

updateEnv :: String -> Int -> TypeEnv -> TypeEnv
updateEnv var loc env = env { variables = Map.insert var loc (variables env) }
lookupEnv :: String -> TypeEnv -> Maybe Int
lookupEnv var = Map.lookup var . variables
clearBlockVars :: TypeEnv -> TypeEnv
clearBlockVars env = env { thisBlockVars = Map.empty }
updateExpectedReturnType :: Type Pos -> TypeEnv -> TypeEnv
updateExpectedReturnType t env = env { expectedReturnType = t }
updateSelfType :: String -> TypeEnv -> TypeEnv
updateSelfType t env = env { selfType = Just t }
updateEnvWithLoc :: String -> Int -> (Int, Int) -> TypeEnv -> Either String TypeEnv
updateEnvWithLoc var loc pos env = case Map.lookup var tbv of
    Just oldPos -> Left $ "Variable " ++ var ++ " was declared in block already at " ++ show oldPos
    Nothing -> Right $ updateEnv var loc env { thisBlockVars = Map.insert var pos tbv }
    where tbv = thisBlockVars env

loadClassFields :: String -> TypeEnv -> TypeEnv
loadClassFields className env = env { fields = classFields $ classesInfo env Map.! className }
lookupSelfField :: String -> TypeEnv -> Maybe (Type Pos)
lookupSelfField fieldName = Map.lookup fieldName . fields
lookupFunction :: String -> TypeEnv -> Maybe (FunType Pos)
lookupFunction funName = Map.lookup funName . functions
lookupField :: String -> String -> TypeEnv -> Maybe (Type Pos)
lookupField className fieldName env = lookupClassField fieldName $ getClassDefinition (classesInfo env) className
lookupMethod :: String -> String -> TypeEnv -> Maybe (FunType Pos)
lookupMethod className methodName env = Map.lookup className (methods env) >>= Map.lookup methodName

-- Function names should include built-in functions.
initEnv :: ClassHierarchy -> FunctionSignatures -> ClassesInfo -> TypeEnv
initEnv hierarchy funSignatures classesInfo = TypeEnv
    { variables = Map.empty
    , functions = funSignatures
    , fields = Map.empty
    , methods = Map.map classMethods classesInfo
    , expectedReturnType = undefined
    , selfType = Nothing
    , thisBlockVars = Map.empty
    , errTrace = []
    , hierarchy = hierarchy
    , classesInfo = classesInfo
    }

initStore :: Store (Type Pos)
initStore = mkStore Map.empty

type TypeM = SemM (Type Pos) TypeEnv

lookupStoreM :: Int -> TypeM (Type Pos)
lookupStoreM loc = gets (lookupStore loc) >>= maybe
    (throwTrace "FATAL ERROR: loc pointing to unallocated memory")
    return

withTraceM :: String -> TypeM TypeEnv -> TypeM TypeEnv
withTraceM msg action = dropTrace <$> local (updateTrace msg) action

typeExistsM :: Type Pos -> TypeM ()
typeExistsM t = case t of
    ClassType{} -> classExistsM $ name t
    _ -> return ()

classExistsM :: String -> TypeM ()
classExistsM className = do
    h <- asks hierarchy
    unless (classExists className h) (throwTrace $ "Class " ++ className ++ " does not exist")

checkTopDef :: TopDef Pos -> TypeM ()
checkTopDef def = case def of
    TopFunDef _ funDef
        -> local (updateTrace $ "in definition of function " ++ description funDef) (checkFunDef funDef)
    BaseClassDef _ _ members -> checkMembers (name def) (description def) members
    ExtClassDef _ _ _ members -> checkMembers (name def) (description def) members
    where checkMember = \case
            ClassFieldDef{} -> return () -- checked in ClassAnalysis
            ClassMethodDef _ funDef
                -> local (updateTrace $ "in definition of method " ++ description funDef) (checkFunDef funDef)
          checkMembers :: String -> String -> [ClassMember Pos] -> TypeM ()
          checkMembers className description members = 
            let traceMsg = "in definition of class " ++ description
            in local (updateTrace traceMsg . updateSelfType className . loadClassFields className)
                     (mapM_ checkMember members)

checkFunDef :: FunDef Pos -> TypeM ()
checkFunDef def = do
    funNamePref <- maybe "function" (const "method") <$> asks selfType
    -- this will detect duplicated arguments of function
    env <- withTraceM ("in declaration of arguments of " ++ funNamePref)
                      (ask >>= flip (foldM addArgDecl) (funArgs def))
    local (const $ updateExpectedReturnType (funRetType def) $ updateTrace ("in body of " ++ funNamePref) env)
          (checkStmtBlock $ funBlock def)
    -- Return checker works after optimizations
    where addArgDecl baseEnv arg = local (const baseEnv) (addVarDecl (position arg) (argType arg) (name arg))

checkStmtBlock :: Block Pos -> TypeM ()
checkStmtBlock block = do
    env <- asks $ updateTrace ("in " ++ description block) . clearBlockVars
    foldM_ checkStmt' env (blockStmts block)
    where checkStmt' baseEnv stmt = local (const baseEnv) (checkStmt stmt)

isNotDecl :: Stmt Pos -> TypeM ()
isNotDecl = \case
    SDecl _ _ -> throwTrace "Declaration is not allowed here, wrap it with braces if this is really what you intend"
    _ -> return ()

checkStmt :: Stmt Pos -> TypeM TypeEnv
checkStmt stmt = case stmt of
    SEmpty _ -> ask
    SBlock _ block -> checkStmtBlock block >> ask
    SDecl _ decl -> withT (checkDecl decl)
    SAssign _ lval exp -> withT $ do
        isUpdateable lval "assign to"
        lvalType <- local (updateTrace $ "in left hand side of " ++ name stmt) (checkLValType lval)
        expType <- local (updateTrace $ "in right hand side of " ++ name stmt) (checkExpType exp)
        h <- asks hierarchy
        if isValidAssignment h expType lvalType
            then ask
            else throwTrace $ "Trying to assign expression of type " ++ name expType
                        ++ " to variable of type " ++ name lvalType
    SIncr _ lval -> withT (xcrement "in" lval)
    SDecr _ lval -> withT (xcrement "de" lval)
    SRet _ exp -> withT $ do
        retType <- asks expectedReturnType
        when (retType == voidType) (throwTrace "Trying to return value, but no value is expected")
        expType <- checkExpType exp
        h <- asks hierarchy
        if isValidAssignment h expType retType
            then ask
            else throwTrace $ "Trying to return value of type " ++ name expType
                            ++ ", expected " ++ name retType
    SRetVoid _ -> withT $ do
        t <- asks expectedReturnType
        if t == voidType
            then ask
            else throwTrace $ "Trying to return no value, but value of type " ++ name t ++ " is expected"
    SIf _ exp ifStmt -> withT
         $ isNotDecl ifStmt
        >> checkCondStmt exp (local (updateTrace "in body of if condition") (checkStmt ifStmt))
    SIfte _ exp trueStmt falseStmt -> withT
         $ isNotDecl trueStmt >> isNotDecl falseStmt
        >> checkCondStmt exp
             ( local (updateTrace "in positive branch of if-else") (checkStmt trueStmt)
            >> local (updateTrace "in negative branch of if-else") (checkStmt falseStmt) )
    SWhile _ exp loopStmt -> withT
         $ isNotDecl loopStmt
        >> checkCondStmt exp (local (updateTrace "in body of while loop") (checkStmt loopStmt))
    SFor _ identT ident exp loopStmt -> withT $ do
        isNotDecl loopStmt
        expType <- checkExpType exp
        h <- asks hierarchy
        case expType of
            ArrayType _ elemT -> if isValidAssignment h elemT identT
                then do newEnv <- addVarDecl (position stmt) elemT (unIdent ident)
                        local (const newEnv) (checkStmt loopStmt)
                        ask
                else throwTrace $ "Types mismatched in for loop: array elements have type " ++ name elemT
                                ++ ", variable " ++ unIdent ident ++ " has type " ++ name identT
            _ -> throwTrace $ "Trying to use for loop on value of type " ++ name expType
                            ++ ", expected any array"
    SExp _ exp -> withT $ do
        t <- checkExpType exp
        if t == voidType
            then ask
            else throwTrace $ "Ignoring value of type " ++ name t
    where withT = withTraceM ("in " ++ description stmt)
          isUpdateable lval name = case lval of
            LValSelf{} -> throwTrace $ "Trying to " ++ name ++ " self"
            LValCall{} -> throwTrace $ "Trying to " ++ name ++ " value returned by function"
            LValMethod{} -> throwTrace $ "Trying to " ++ name ++ " value returned by method"
            LValAttr _ lval _ -> checkLValType lval >>= \case
                ArrayType{} -> throwTrace $ "Trying to " ++ name ++ " length of the array"
                _ -> return ()
            _ -> return ()
          xcrement x lval = let ment = x ++ "crement" in do
            isUpdateable lval ment
            lvalType <- checkLValType lval
            if lvalType == intType
                then ask
                else throwTrace $ "Trying to " ++ ment ++ " expression of type " ++ name lvalType
                                ++ ", expected " ++ name intType
          checkCondStmt exp action = do
            t <- checkExpType exp
            if t == boolType
                then action >> ask
                else throwTrace $ "Condition has type " ++ name t ++ ", expected " ++ name boolType

addVarDecl :: (Int, Int) -> Type Pos -> String -> TypeM TypeEnv
addVarDecl pos t var = withTraceM ("in declaration of variable " ++ var ++ " at " ++ show pos) $ do
    loc <- allocInsertM t
    env <- ask
    case updateEnvWithLoc var loc pos env of -- this detects declaring variable twice in a block
        Left err -> throwTrace err
        Right newEnv -> return newEnv

checkDeclItem :: Type Pos -> DeclItem Pos -> TypeM TypeEnv
checkDeclItem t decl = case decl of
    DeclNoInit _ ident -> addVarDecl (position decl) t (unIdent ident)
    DeclWithInit _ _ exp -> do
        expType <- checkExpType exp
        h <- asks hierarchy
        if isValidAssignment h expType t
            then addVarDecl (position decl) t (name decl)
            else throwTrace $ "Trying to assign expression of type " ++ name expType
                           ++ " to variable of type " ++ description t

checkDecl :: Decl Pos -> TypeM TypeEnv
checkDecl decl
    | t == voidType = throwTrace "Trying to declare variables of void type"
    | otherwise = typeExistsM t >> runDecls
    where t = declType decl
          checkItem baseEnv declItem = local (const baseEnv) (checkDeclItem t declItem)
          runDecls = ask >>= flip (foldM checkItem) (declItems decl)

checkLValType :: LVal Pos -> TypeM (Type Pos)
checkLValType = \case
    LValSelf _ -> asks selfType >>= maybe (throwTrace "Keyword self used outside of class")
                                          (return . classType)
    LValVar _ ident -> asks (lookupEnv varName) >>= maybe
        -- Variable does not exist - check for class field (if we are in method)
        (asks (lookupSelfField varName) >>= maybe
            (throwTrace $ "Variable " ++ varName ++ " is not defined")
            return
        )
        lookupStoreM -- Variable exists
        where varName = unIdent ident
    LValAttr _ lval ident -> checkLValType lval >>= \t -> case t of
        ArrayType{} -> if ident == lengthIdent
            then return intType
            else throwTrace "Arrays support only length atribute"
        ClassType{} -> asks (lookupField className fieldName) >>= maybe
            (throwTrace $ "Class " ++ className ++ " does not have field " ++ fieldName)
            return
            where className = name t
        where fieldName = unIdent ident
    LValCall _ fun@(FunCall _ funIdent exps) -> asks (lookupFunction funName) >>= maybe
        (throwTrace $ "Trying to call nonexistent function " ++ description fun)
        (\t -> local (updateTrace $ "when calling function " ++ description fun) (checkFunApp t exps))
        where funName = unIdent funIdent
    LValMethod _ lval fun@(FunCall _ methodIdent exps) -> checkLValType lval >>= \t -> case t of
        ArrayType{} -> throwTrace "Arrays do not have methods"
        ClassType{} -> asks (lookupMethod className methodName) >>= maybe
            (throwTrace $ "Trying to call nonexistent " ++ methodDescription)
            (\t -> local (updateTrace $ "when calling " ++ methodDescription) (checkFunApp t exps))
            where className = name t
                  methodName = unIdent methodIdent
                  methodDescription = "method " ++ methodName ++ " of class " ++ className
                                   ++ " at " ++ show (position fun)
    LValArrAccess _ lval exp -> checkLValType lval >>= \case
        ArrayType _ elType -> checkExpType exp >>= \case 
            IntType _ -> return elType
            t -> throwTrace $ "Trying to access element of array " ++ description lval ++ " with expression of type "
                           ++ name t ++ ", expected " ++ name intType
        t -> throwTrace $ "Trying to use array access operator on element of type " ++ name t
    where checkFunApp funType exps = do
            let expsl = length exps
            let argsl = length (argTypes funType)
            unless (expsl == argsl) $ throwTrace $ "Number of arguments applied is incorrect: expected "
                ++ show argsl ++ ", got " ++ show expsl
            expTypes <- mapM checkExpType exps
            h <- asks hierarchy
            let matching = zipWith (isValidAssignment h) expTypes (argTypes funType)
            unless (and matching) $ throwTrace $ "Types of argument do not match: expected "
                ++ nameManyTypes (argTypes funType) ++ ", got " ++ nameManyTypes expTypes
            return $ retType funType
          getClassName lval = checkLValType lval >>= \t -> case t of
            ClassType{} -> return $ name t
            _ -> throwTrace $ "Trying to use dot operator on value of type " ++ name t

checkExpType :: Exp Pos -> TypeM (Type Pos)
checkExpType exp = case exp of
    ENullObj _ ident -> withT (checkClass ident)
    ELVal _ lval -> checkLValType lval
    ENewObj _ ident -> withT (checkClass ident)
    ENewArr _ t exp -> withT $ case t of
        ArrayType{} -> throwTrace "Language supports only one dimensional arrays"
        _ -> checkExpType exp >>= \case
            IntType _ -> return $ arrayType t
            t -> throwTrace $ "Size of array has type " ++ name t ++ ", expected " ++ name intType
    EConstant _ (CInteger _ _) -> return intType
    EConstant _ (CString _ _) -> return stringType
    EConstant _ _ -> return boolType
    ENegate _ op exp -> do
        expType <- checkExpType exp
        let expectedType = case op of { OpNegate _ -> intType; OpNot _ -> boolType }
        if expType == expectedType
            then return expType
            else throwTrace $ "Trying to negate expression of type " ++ name expType
                            ++ ", expected " ++ name expectedType
    EMod _ e1 op e2 -> checkUnitypedOp op intType intType e1 e2
    EAdd _ e1 op e2 -> case op of
        OpMinus _ -> checkUnitypedOp opMinus intType intType e1 e2
        OpPlus _ -> checkExpType e1 >>= \expType1 -> if expType1 `elem` [intType, stringType]
            then do expType2 <- checkExpType e2
                    unless (expType1 == expType2) (errMsg "Second" op expType1 expType2)
                    return expType1
            else throwTrace $ "First argument of + should have type int or string, but it has type "
                           ++ name expType1
    EComp _ e1 op e2 -> if op `elem` [OpEqual Nothing, OpNeq Nothing]
        then do expType1 <- local (updateTrace "in left hand side of equality check") (checkExpType e1)
                expType2 <- local (updateTrace "in right hand side of equality check") (checkExpType e2)
                h <- asks hierarchy
                if isValidAssignment h expType1 expType2 || isValidAssignment h expType2 expType1
                    then return boolType
                    else throwTrace $ "Trying to check equality of two noncomparable types: "
                                   ++ name expType1 ++ " and " ++ name expType2
        else checkUnitypedOp op intType boolType e1 e2
    EAnd _ e1 e2 -> checkUnitypedOp OpAnd boolType boolType e1 e2
    EOr _ e1 e2 -> checkUnitypedOp OpOr boolType boolType e1 e2
    where withT = local (updateTrace $ "when creating " ++ description exp)
          errMsg which op expected t = throwTrace $ which ++ " argument of " ++ name op
                ++ " should have type " ++ name expected ++ ", but it has type " ++ name t
          checkUnitypedOp op argsType retType e1 e2 = do
                expType1 <- checkExpType e1
                unless (expType1 == argsType) (errMsg "First" op argsType expType1)
                expType2 <- checkExpType e2
                unless (expType2 == argsType) (errMsg "Second" op argsType expType2)
                return retType
          checkClass (Ident className) = do
                classExistsM className
                return (classType className)

checkProgram :: Program Pos -> TypeM ()
checkProgram (Program _ defs) = mapM_ checkTopDef defs

typecheck :: Program Pos -> ClassHierarchy -> ClassesInfo -> FunctionSignatures -> Either String ()
typecheck program hierarchy classesInfo signatures
    = runSemM (checkProgram program) initStore $ initEnv hierarchy signatures classesInfo