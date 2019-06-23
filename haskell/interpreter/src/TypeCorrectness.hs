module TypeCorrectness where

import Control.Monad
import qualified Control.Monad.Except as E
import Control.Monad.State.Lazy
import Control.Monad.Reader

import AbsGrammar

import MemoryContent
import Utils

------------------------------------------------------------------------

type ErrorTrace = [String]

-- TypeEnv fields:
-- * standard environment
-- * memory location of current function
-- * detailed information about error
-- * are we in loop? This allows us to use break and continue.
type TypeEnv = (Env, Loc, ErrorTrace, Bool)

updateTypeEnv :: Var -> Loc -> Loc -> String -> TypeEnv -> TypeEnv
updateTypeEnv v newloc funLoc err (env, _, errTrace, p) = 
    (updateEnv v newloc env, funLoc, eConcat err errTrace, p)

updateErrorTrace :: String -> TypeEnv -> TypeEnv
updateErrorTrace err (env, loc, errTrace, p) =
    (env, loc, eConcat err errTrace, p)

getInLoop :: TypeEnv -> TypeEnv
getInLoop (env, loc, errTrace, _) = (env, loc, errTrace, True)

getOutOfLoop :: TypeEnv -> TypeEnv
getOutOfLoop (env, loc, errTrace, _) = (env, loc, errTrace, False)

type TypeMonad a = StateT TypeStore (ReaderT TypeEnv (E.Except ErrorTrace)) a 

throwError :: String -> TypeMonad a
throwError s = do
    (_, _, e, _) <- ask
    E.throwError $ s:e

getLoc :: Var -> Env -> TypeMonad Loc
getLoc v env = case lookupEnv v env of
    Just loc -> return loc
    Nothing -> throwError $ "Could not find variable " ++ v
                            ++ " in environment"

getMem :: Loc -> TypeStore -> TypeMonad MemoryType
getMem loc store = case lookupStore loc store of
    Just mem -> return mem
    Nothing -> throwError $ "Could not find contents at position "
                            ++ show loc ++ ", probably trying to return in main"

------------------------------------------------------------------------

checkExpType :: Exp -> TypeMonad ExpType
checkExpType (EVar pv) = do
    let v = getFromPIdent pv
    (env, _, _, _) <- ask
    store <- get
    loc <- getLoc v env
    memcell <- getMem loc store
    case memcell of
        MemVarType t -> return $ ExpType t
        otherwise -> throwError $
            "Identifier " ++ prettyPShow pv ++ " is not a variable"

checkExpType (ECall pf argExps) = do
    let f = getFromPIdent pf
    (env, _, _, _) <- ask
    store <- get
    loc <- getLoc f env
    memcell <- getMem loc store
    case memcell of
        MemFuncType memf -> do
            argTypes <- mapM checkExpType argExps
            let checkArgType (e1, e2, argNr) =
                    when (e1 /= e2) $ throwError $
                    "Error in call of " ++ prettyPShow pf
                    ++ ": type of argument number " ++ show argNr
                    ++ " does not match: got " ++ prettyExpType e1
                    ++ ", expected " ++ prettyExpType e2
            mapM_ checkArgType $ zip3 argTypes (justArgTypes memf) [1..]
            let refCheck (funcArg, argExp, argNr) = case funcArg of
                    FuncArgVal _ -> return ()
                    FuncArgRef _ -> case argExp of
                        EVar _ -> return ()
                        _ -> throwError $ "Error in call of " ++ prettyPShow pf
                            ++ ": it takes reference at argument number "
                            ++ show argNr
                            ++ ", so expression must be variable name"
            mapM_ refCheck $ zip3 (funcArgs memf) argExps [1..]
            return $ returnType memf
        _ -> throwError $ "Error in call of " ++ prettyPShow pf
                          ++ ": it is not a function"

checkExpType (EInteger _) = return $ ExpType IntegerType
checkExpType (EDouble _) = return $ ExpType DoubleType
checkExpType (EString _) = return $ ExpType StringType
checkExpType ETrue = return $ ExpType BoolType
checkExpType EFalse = return $ ExpType BoolType

checkExpType (ENegate e) = do
    e <- checkExpType e
    if elem e $ map ExpType [IntegerType, DoubleType, BoolType]
        then return e
        else throwError $ "Invalid argument for negation operator: "
                          ++ "got type " ++ prettyExpType e

checkExpType (EMod e1 e2) = do
    e1 <- checkExpType e1
    e2 <- checkExpType e2
    if e1 /= ExpType IntegerType
        then throwError $ "First argument of % is not an Integer"
        else if e2 /= e1
            then throwError $ "Second argument of % is not an Integer"
            else return e1

checkExpType (EMul e1 e2) = do
    e1 <- checkExpType e1
    e2 <- checkExpType e2
    if e1 /= e2
        then throwError $ "Argument types do not match for `*` ("
                          ++ prettyExpType e1 ++ " =/= "
                          ++ prettyExpType e2 ++ ")"
        else if elem e1 $ map ExpType [IntegerType, DoubleType]
            then return e1
            else throwError $ "Invalid argument types for `*` (can't" ++
                              " multiply " ++ prettyExpType e1 ++ "s)"

checkExpType (EDiv e1 e2) = do
    e1 <- checkExpType e1
    e2 <- checkExpType e2
    if e1 /= e2
        then throwError $ "Argument types do not match for `/` ("
                          ++ prettyExpType e1 ++ " =/= "
                          ++ prettyExpType e2 ++ ")"
        else if elem e1 $ map ExpType [IntegerType, DoubleType]
            then return e1
            else throwError $ "Invalid argument types for `/` (can't" ++
                              " divide " ++ prettyExpType e1 ++ "s)"

checkExpType (EAdd e1 e2) = do
    e1 <- checkExpType e1
    e2 <- checkExpType e2
    if e1 /= e2
        then throwError $ "Argument types do not match for `+` ("
                          ++ prettyExpType e1 ++ " =/= "
                          ++ prettyExpType e2 ++ ")"
        else if elem e1 $ map ExpType [IntegerType, DoubleType, StringType]
            then return e1
            else throwError $ "Invalid argument types for `+` (can't" ++
                              " add " ++ prettyExpType e1 ++ "s)"

checkExpType (ESub e1 e2) = do
    e1 <- checkExpType e1
    e2 <- checkExpType e2
    if e1 /= e2
        then throwError $ "Argument types do not match for `-` ("
                          ++ prettyExpType e1 ++ " =/= "
                          ++ prettyExpType e2 ++ ")"
        else if elem e1 $ map ExpType [IntegerType, DoubleType]
            then return e1
            else throwError $ "Invalid argument types for `-` (can't" ++
                              " subtract " ++ prettyExpType e1 ++ "s)"

checkExpType (EComp e1 _ e2) = do
    e1 <- checkExpType e1
    e2 <- checkExpType e2
    if e1 /= e2
        then throwError $ "Argument types do not match in comparison ("
                          ++ prettyExpType e1 ++ " =/= "
                          ++ prettyExpType e2 ++ ")"
        else if elem e1 $ map ExpType [IntegerType, DoubleType]
            then return $ ExpType BoolType
            else throwError $ "Invalid argument types for comparison "
                              ++ "(can't compare " ++ prettyExpType e1
                              ++ "s)"

checkExpType (EOr e1 e2) = do
    e1 <- checkExpType e1
    e2 <- checkExpType e2
    if e1 == ExpType BoolType && e2 == e1
        then return e1
        else throwError $ "Argument types in || are not Bool"

checkExpType (EAnd e1 e2) = do
    e1 <- checkExpType e1
    e2 <- checkExpType e2
    if e1 == ExpType BoolType && e2 == e1
        then return $ ExpType BoolType
        else throwError $ "Argument types in && are not Bool"

checkExpType (EEq e1 e2) = do
    e1 <- checkExpType e1
    e2 <- checkExpType e2
    if e1 /= VoidType && e1 == e2
        then return $ ExpType BoolType
        else throwError $ "Argument types do not match in =="

checkExpType (ENeq e1 e2) = do
    e1 <- checkExpType e1
    e2 <- checkExpType e2
    if e1 /= VoidType && e1 == e2
        then return $ ExpType BoolType
        else throwError $ "Argument types do not match in !="

------------------------------------------------------------------------

checkDeclCorrectness :: Decl -> TypeMonad TypeEnv
checkDeclCorrectness (FuncDecl pf funcArgs retType argPIdents block) = do
    let f = getFromPIdent pf
    let argIdents = map getFromPIdent argPIdents
    when (hasDuplicates argIdents) $ throwError $
        "Duplicated identifiers in declaration of " ++ prettyPShow pf
    when (length funcArgs /= length argPIdents) $ throwError $
        "Amount of passed arguments does not match amount of " ++
        "declared arguments in declaration of " ++ prettyPShow pf
    (env, oldRetLoc, _, _) <- ask
    store <- get
    let newloc = alloc store
        memcontent = MemFuncType $ FuncInMem funcArgs [] retType [] (FuncBody block) env
        argzip = zip (map (justVarType . justArgType) funcArgs) argIdents in do
        modify $ updateStore newloc memcontent
        -- For typechecking ref/val is irrelevant
        let singleAlloc :: (VarType, Var) -> TypeMonad (Loc, Var)
            singleAlloc (t, x) = do
            store <- get
            let l = alloc store
            modify $ updateStore l $ MemVarType t -- value is irrelevant
            return (l, x)
        argzip <- mapM singleAlloc argzip
        let singleEnvChange (l, x) = updateTypeEnv x l newloc ""
        let argsEnvChange = compose $ map singleEnvChange argzip
        let errorTrace = "in body of function " ++ prettyPShow pf
        local
            (argsEnvChange . (updateTypeEnv f newloc newloc errorTrace)
             . getOutOfLoop)
            $ checkStmtBlockCorrectness block
        ask >>= return . updateTypeEnv f newloc oldRetLoc ""

checkDeclCorrectness (VarDecl varType pv e) = do
    let v = getFromPIdent pv
    let trace = "in declaration of variable " ++ prettyPShow pv
    (env, oldRetLoc, _, _) <- ask
    store <- get
    eType <- local (updateErrorTrace trace) $ checkExpType e
    if ExpType varType == eType
        then do
            let newloc = alloc store
                memcontent = MemVarType varType -- value is irrelevant
            modify $ updateStore newloc memcontent
            ask >>= return . updateTypeEnv v newloc oldRetLoc ""
        else throwError $ "Invalid type of expression " ++ trace ++
                          ": got " ++ prettyExpType eType ++
                          ", expected " ++ prettyVarType varType

checkDeclCorrectness (StaticVarDecl varType v e) = 
    checkDeclCorrectness (VarDecl varType v e) -- static is irrelevant

------------------------------------------------------------------------

checkStmtBlockCorrectness :: StmtBlock -> TypeMonad ()
checkStmtBlockCorrectness (StmtBlock []) = return ()
checkStmtBlockCorrectness (StmtBlock (stmt:stmts)) = do
    env <- checkStmtCorrectness stmt
    local (const env) $ checkStmtBlockCorrectness (StmtBlock stmts)

-- Every expression must evaluate to type '()'
-- Return value of function must not be ignored
checkStmtCorrectness :: Stmt -> TypeMonad TypeEnv
checkStmtCorrectness (SDecl d) = checkDeclCorrectness d

checkStmtCorrectness (SExp e) = do
    eType <- checkExpType e
    unless (eType == VoidType) $ throwError $ "Value of expression is ignored"
    ask >>= return

checkStmtCorrectness (SBlock b) = do
    checkStmtBlockCorrectness b
    ask >>= return

checkStmtCorrectness (SAssign pv e) = do
    let v = getFromPIdent pv
    let extTrace = local $ updateErrorTrace $
            "in assignment to variable " ++ prettyPShow pv
    eType <- checkExpType e
    (env, _, _, _) <- ask
    store <- get
    loc <- extTrace $ getLoc v env
    memcell <- extTrace $ getMem loc store
    case memcell of
        MemVarType t -> if eType == ExpType t
            then ask >>= return
            else throwError $ "Trying to assign to " ++ prettyPShow pv
                              ++ ", but type of expression "
                              ++ prettyExpType eType
                              ++ " does not match type of variable: "
                              ++ prettyVarType t
        _ -> throwError $ "Trying to assign to " ++ prettyPShow pv
                          ++ ", which is not a variable"

checkStmtCorrectness (SIf e b) = do
    eType <- checkExpType e
    if eType == ExpType BoolType
        then do
            local (updateErrorTrace "in if block (without else)")
                $ checkStmtBlockCorrectness b
            ask >>= return
        else throwError $ "If condition must have Bool type, but it has"
                          ++ prettyExpType eType

checkStmtCorrectness (SIfte e b1 b2) = do
    eType <- checkExpType e
    if eType == ExpType BoolType
        then do 
            local (updateErrorTrace "in if block (with else)")
                $ checkStmtBlockCorrectness b1
            local (updateErrorTrace "in else")
                $ checkStmtBlockCorrectness b2
            ask >>= return
        else throwError $ "Ifte condition must have Bool type, but it has"
                          ++ prettyExpType eType

checkStmtCorrectness (SWhile e b) = do
    eType <- checkExpType e
    if eType == ExpType BoolType
        then do 
            local (updateErrorTrace "in loop" . getInLoop)
                $ checkStmtBlockCorrectness b
            ask >>= return
        else throwError $ "While condition must have Bool type, but it has"
                          ++ prettyExpType eType

checkStmtCorrectness SBreak = do
    (_, _, _, p) <- ask
    unless p $ throwError "Used break outside of loop"
    ask >>= return

checkStmtCorrectness SContinue = do
    (_, _, _, p) <- ask
    unless p $ throwError "Used continue outside of loop"
    ask >>= return

checkStmtCorrectness (StmtRet e) = do
    eType <- checkExpType e
    (_, funLoc, _, _) <- ask
    store <- get
    memcell <- getMem funLoc store
    case memcell of
        MemFuncType memf -> do
            unless (returnType memf == eType) $
                throwError $ "Trying to return value of wrong type: got "
                             ++ prettyExpType eType ++ ", expected "
                             ++ prettyExpType (returnType memf)
            ask >>= return
        _ -> throwError "Memory error: expected function"

checkStmtCorrectness StmtRetV = do
    (_, funLoc, _, _) <- ask
    store <- get
    memcell <- getMem funLoc store
    case memcell of
        MemFuncType memf -> do
            unless (returnType memf == VoidType) $
                throwError $ "Trying to return value of wrong type: got"
                             ++ " (), expected "
                             ++ prettyExpType (returnType memf)
            ask >>= return
        _ -> throwError "Memory error: expected function"

checkStmtCorrectness (STry block catches) = do
    when (hasDuplicates $ map (\(SCatch v _) -> v) catches) $ throwError $
        "Duplicated expected exceptions"
    local (updateErrorTrace "in try block") $ checkStmtBlockCorrectness block
    mapM_  checkCatchCorrectness catches
    ask >>= return

checkStmtCorrectness (SThrow _) = ask >>= return

checkCatchCorrectness (SCatch pe b) = 
    let trace = "in catch block " ++ prettyPShow pe in
        local (updateErrorTrace trace) $ checkStmtBlockCorrectness b

checkProgTypeCorrectness :: Prog -> Either ErrorTrace ()
checkProgTypeCorrectness (Prog decls (StmtBlock main)) =
    E.runExcept $ runReaderT (evalStateT check initTypeStore)
                  (initEnv, nullloc, [], False) where
        check = checkStmtBlockCorrectness $ StmtBlock $
                map SDecl decls ++ main
