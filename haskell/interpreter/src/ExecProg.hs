module ExecProg where

import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Except
import System.IO(stdin, hPutStrLn, stderr)
import System.Exit(exitFailure, exitSuccess)
import Data.Maybe
import qualified Text.Read as R
import Data.Typeable
import Data.List

import qualified Data.Map.Strict as Map

import AbsGrammar
import MemoryContent

import Utils

------------------------------------------------------------------------

printErr = hPutStrLn stderr

-- ExecEnv fields:
-- * standard environment
-- * location of currently executed function
-- * Are we in main statement block of function? We need that information
--   to not remember static variables declared in inner scopes of function.
data ControlFlow
    = Exception String
    | Break | Continue
    | ReturnVoid | ReturnValue MemoryContent
type ExecEnv = (Env, Loc, Bool)
updateEEnv :: Var -> Loc -> ExecEnv -> ExecEnv
updateEEnv v newloc (env, l, b) = (updateEnv v newloc env, l, b)
updateELoc :: Loc -> ExecEnv -> ExecEnv
updateELoc l (e, _, b) = (e, l, b)
updateEOutF :: ExecEnv -> ExecEnv
updateEOutF (e, l, _) = (e, l, False)

type ExecMonad a = ExceptT ControlFlow (StateT ExecStore (ReaderT ExecEnv IO)) a

getLoc :: Var -> Env -> ExecMonad Loc
getLoc v env = case lookupEnv v env of Just loc -> return loc

getMem :: Loc -> ExecStore -> ExecMonad MemoryContent
getMem loc store = case lookupStore loc store of Just mem -> return mem

------------------------------------------------------------------------

execExp :: Exp -> ExecMonad MemoryContent
execExp (EVar pv) = do
    let v = getFromPIdent pv
    (env, _, _) <- ask
    store <- get
    loc <- getLoc v env
    memcell <- getMem loc store
    return memcell

execExp (ECall pf argExps) = do
    let f = getFromPIdent pf
    (env, _, _) <- ask
    store <- get
    funloc <- getLoc f env
    MemFunc memf <- getMem funloc store
    case funcBody memf of
        GetInput -> liftIO getLine >>= return . String
        Print -> do
            e <- execExp (head argExps)
            case e of
                String s -> liftIO $ putStrLn s
                Double x -> liftIO $ putStrLn $ show x
                Integer n -> liftIO $ putStrLn $ show n
            return Null
        ToString -> do
            e <- execExp (head argExps)
            case e of
                Double x -> return $ String $ show x
                Integer n -> return $ String $ show n
        StringToInteger -> do
            String s <- execExp (head argExps)
            case R.readMaybe s of
                Just x -> return $ Integer $ truncate x
                Nothing -> throwError $ Exception "UnparsableNumberException"
        StringToDouble -> do
            String s <- execExp (head argExps)
            case R.readMaybe s of
                Just x -> return $ Double $ x
                Nothing -> throwError $ Exception "UnparsableNumberException"
        FuncBody (StmtBlock block) -> do
        -- Static variables (even initialized ones) are included at declaration
            let singleAlloc (FuncArgVal t, e, v) = do
                    e <- execExp e
                    store <- get
                    let newloc = alloc store
                    modify $ updateStore newloc e
                    return (newloc, v)
                singleAlloc (FuncArgRef t, EVar pw, v) = do
                    (env, _, _) <- ask
                    loc <- getLoc (getFromPIdent pw) env
                    return (loc, v)
            argzip <- mapM singleAlloc $ zip3 (funcArgs memf) argExps (funcArgVars memf)
            let singleEnvChange (l, x) = updateEEnv x l
            let argsEnvChange = compose $ map singleEnvChange argzip
            let funEnv = argsEnvChange (closure memf, funloc, True)
            let funComp = local (const funEnv) $ execStmts block
            ExceptT $ runExceptT funComp >>= \e -> runExceptT $ case e of
                Left (ReturnValue e) -> return e
                Left ReturnVoid -> return Null
                Left x -> throwError x
                Right () -> case returnType memf of
                    VoidType -> return Null
                    _ -> throwError $ Exception "NoReturnException"
    

execExp (EInteger n) = return $ Integer n
execExp (EDouble x) = return $ Double x
execExp (EString s) = return $ String s
execExp ETrue = return $ Bool True
execExp EFalse = return $ Bool False

execExp (ENegate e) = do
    e <- execExp e
    case e of
        Bool p -> return $ Bool $ not p
        Integer n -> return $ Integer $ negate n
        Double x -> return $ Double $ negate x

execExp (EMod e1 e2) = do
    Integer n1 <- execExp e1
    Integer n2 <- execExp e2
    when (n2 <= 0) $ throwError $ Exception "NonpositiveModuloException"
    return $ Integer $ n1 `mod` n2

execExp (EMul e1 e2) = do
    e1 <- execExp e1
    e2 <- execExp e2
    case (e1, e2) of
        (Integer n1, Integer n2) -> return $ Integer $ n1 * n2
        (Double x1, Double x2) -> return $ Double $ x1 * x2

execExp (EDiv e1 e2) = do
    e1 <- execExp e1
    e2 <- execExp e2
    case (e1, e2) of
        (Integer n1, Integer n2) -> do
            when (n2 == 0) $ throwError $ Exception "DivByZeroIntegerException"
            return $ Integer $ n1 `div` n2
        (Double x1, Double x2) -> do
            when (x2 == 0.0) $ throwError $ Exception "DivByZeroDoubleException"
            return $ Double $ x1 / x2

execExp (EAdd e1 e2) = do
    e1 <- execExp e1
    e2 <- execExp e2
    case (e1, e2) of
        (Integer n1, Integer n2) -> return $ Integer $ n1 + n2
        (Double x1, Double x2) -> return $ Double $ x1 + x2
        (String s1, String s2) -> return $ String $ s1 ++ s2

execExp (ESub e1 e2) = do
    e1 <- execExp e1
    e2 <- execExp e2
    case (e1, e2) of
        (Integer n1, Integer n2) -> return $ Integer $ n1 - n2
        (Double x1, Double x2) -> return $ Double $ x1 - x2

execExp (EComp e1 op e2) = do
    e1 <- execExp e1
    e2 <- execExp e2
    let f :: Ord a => a -> a -> Bool
        f = case op of
                OpLess -> (<)
                OpLeq -> (<=)
                OpGreater -> (>)
                OpGeq -> (<=)
    case (e1, e2) of
        (Integer n1, Integer n2) -> return $ Bool $ f n1 n2
        (Double x1, Double x2) -> return $ Bool $ f x1 x2

execExp (EOr e1 e2) = do
    Bool b1 <- execExp e1
    if b1
        then return $ Bool True
        else execExp e2

execExp (EAnd e1 e2) = do
    Bool b1 <- execExp e1
    if not b1
        then return $ Bool False
        else execExp e2

execExp (EEq e1 e2) = do
    e1 <- execExp e1
    e2 <- execExp e2
    case (e1, e2) of
        (Integer n1, Integer n2) -> return $ Bool $ n1 == n2
        (Double x1, Double x2) -> return $ Bool $ x1 == x2
        (String s1, String s2) -> return $ Bool $ s1 == s2
        (Bool b1, Bool b2) -> return $ Bool $ b1 == b2

execExp (ENeq e1 e2) = do
    (Bool b) <- execExp (EEq e1 e2)
    return $ Bool $ not b

------------------------------------------------------------------------

execDecl :: Decl -> ExecMonad ExecEnv
execDecl (FuncDecl pf funcArgs retType argPIdents block) = 
    let f = getFromPIdent pf
        funcArgVars = map getFromPIdent argPIdents in do
        (env, oldRet, b) <- ask
        store <- get
        let newloc = alloc store
            modEnv = updateEnv f newloc env
            memf = MemFunc $ FuncInMem funcArgs funcArgVars retType [] (FuncBody block) modEnv in do
            modify $ updateStore newloc memf
            return (modEnv, oldRet, b)

execDecl (VarDecl varType pv e) = do
    let v = getFromPIdent pv
    e <- execExp e
    store <- get
    let newloc = alloc store
    modify $ updateStore newloc e
    ask >>= return . updateEEnv v newloc

execDecl (StaticVarDecl varType pv@(PIdent ((x, y), v)) e) = do
    (env, funloc, b) <- ask
    -- static keyword has no meaning outside of function
    if funloc == nullloc || not b
        then execDecl (VarDecl varType pv e)
        else do
            store <- get
            MemFunc memf <- getMem funloc store
            case find (\sv -> declPos sv == (x, y)) (staticVars memf) of
                Just sv -> ask >>= return . updateEEnv v (loc sv)
                Nothing -> do
                    let newloc = alloc store
                    e <- execExp e
                    modify $ updateStore newloc e
                    let staticVar = StaticVar (x, y) newloc
                    let newMemf = memf {staticVars = staticVar:(staticVars memf)}
                    modify $ updateStore funloc $ MemFunc newMemf
                    ask >>= return . updateEEnv v newloc

------------------------------------------------------------------------

-- shorthand for entering scope which is not function's main scope
execStmts' block = local updateEOutF $ execStmts block

execStmts :: [Stmt] -> ExecMonad ()
execStmts [] = do
    (_, funloc, _) <- ask
    unless (funloc == nullloc) $ do
    store <- get
    MemFunc memf <- getMem funloc store 
    unless (returnType memf == VoidType) (throwError $ Exception "NoReturnException")

execStmts (stmt:stmts) = case stmt of
    SDecl decl -> execDecl decl >>= \e -> local (const e) $ execStmts stmts
    SExp e -> execExp e >> execStmts stmts
    SBlock (StmtBlock b) -> execStmts' b >> execStmts stmts
    SAssign pv e -> do
        e <- execExp e
        (env, _, _) <- ask
        store <- get
        loc <- getLoc (getFromPIdent pv) env
        modify $ updateStore loc e
        execStmts stmts
    SIf e (StmtBlock block) -> do
        Bool b <- execExp e
        when b $ execStmts' block
        execStmts stmts
    SIfte e (StmtBlock block1) (StmtBlock block2) -> do
        Bool b <- execExp e
        if b then execStmts' block1 else execStmts' block2
        execStmts stmts
    SWhile e (StmtBlock block) -> do
        Bool b <- execExp e
        if b
            then ExceptT $ runExceptT (execStmts' block) >>= \e -> runExceptT $ case e of
                    Left Break -> execStmts stmts
                    Left Continue -> execStmts (stmt:stmts)
                    Left x -> throwError x
                    Right () -> execStmts (stmt:stmts)
            else execStmts stmts
    SBreak -> throwError Break
    SContinue -> throwError Continue
    StmtRet e -> execExp e >>= throwError . ReturnValue
    StmtRetV -> throwError ReturnVoid
    STry (StmtBlock block) catches -> ExceptT $ runExceptT (execStmts' block) >>= \e -> runExceptT $ case e of
        Right () -> execStmts stmts
        Left (Exception e) -> case find (\(SCatch pv _) -> getFromPIdent pv == e) catches of
            Just (SCatch _ (StmtBlock block)) -> execStmts' block >> execStmts stmts 
            Nothing -> throwError $ Exception e
        Left x -> throwError x
    SThrow pe -> throwError $ Exception $ getFromPIdent pe

__execProg :: Prog -> ExecMonad ()
__execProg (Prog [] (StmtBlock b)) = execStmts b
__execProg (Prog (decl:decls) main) = execDecl decl >>=
    \env -> local (const env) $ __execProg (Prog decls main)

execProg :: Prog -> IO ()
execProg prog =
    runReaderT (evalStateT (runExceptT (__execProg prog)) initExecStore)
               (initEnv, nullloc, False) >>=
    \e -> case e of
        Left (Exception e) -> printErr ("Runtime exception: " ++ e) >> exitFailure
        Right () -> exitSuccess
