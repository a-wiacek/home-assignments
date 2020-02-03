module LLVMTranslator(execLLVMTranslate) where
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import AbsInstant
import LLVMData

data LLVMState = LLVMState
    { declaredVars :: Set.Set Ident -- Using undeclared variable will throw error and terminate translator.
    -- To avoid loading variable multiple times, keep register with first load after the most recent store.
    -- Without this, "a=1;a+a+a+a+a" would load a five times.
    , validRegisters :: Map.Map Ident LLVMIdent
    , producedInstructions :: [LLVMInstr] -- For performance reasons, this is reversed list of generated instructions.
    , freshNameCounter :: Int -- Used to generate fresh new variable names.
    }
initLLVMState = LLVMState Set.empty Map.empty [] 0

type LLVMMonad a = ExceptT String (State LLVMState) a

addInstruction :: LLVMInstr -> LLVMMonad ()
addInstruction i = modify (\s -> s { producedInstructions = i : producedInstructions s })

-- Canonic pointer naming.
mkPtr :: Ident -> LLVMIdent
mkPtr (Ident s) = LLVMIdent $ s ++ ".p"

getFreshName :: LLVMMonad LLVMIdent
getFreshName = do
    c <- gets freshNameCounter
    modify (\s -> s { freshNameCounter = freshNameCounter s + 1 })
    return $ LLVMIdent $ '.' : show c

-- Get pointer for variable, allocating if necessary.
getPtr :: Ident -> LLVMMonad LLVMIdent
getPtr id = do
    vars <- gets declaredVars
    let ptr = mkPtr id
    when (id `Set.notMember` vars) $ do
        modify (\s -> s { declaredVars = id `Set.insert` declaredVars s })
        addInstruction (LLVMAlloc ptr)
    return ptr

-- Translate math operation into LLVM statement.
translateMatOp :: LLVMMatOp -> Exp -> Exp -> LLVMMonad LLVMValue
translateMatOp op e1 e2 = do
    arg1 <- translateExp e1
    arg2 <- translateExp e2
    r <- getFreshName
    addInstruction $ LLVMMatOp r op arg1 arg2
    return $ LLVMValueIdent r

-- Translate expression into LLVM statement.
translateExp :: Exp -> LLVMMonad LLVMValue
translateExp (ExpAdd e1 e2) = translateMatOp LLVMAdd e1 e2
translateExp (ExpSub e1 e2) = translateMatOp LLVMSub e1 e2
translateExp (ExpMul e1 e2) = translateMatOp LLVMMul e1 e2
translateExp (ExpDiv e1 e2) = translateMatOp LLVMDiv e1 e2
translateExp (ExpLit n) = return $ LLVMValueConst $ fromInteger n
translateExp (ExpVar id@(Ident rawId)) = do
    vars <- gets declaredVars
    when (id `Set.notMember` vars) $ throwE $ "Aborting: Usage of undeclared variable " ++ rawId
    regs <- gets validRegisters
    case Map.lookup id regs of
        Nothing -> do v <- getFreshName
                      addInstruction $ LLVMLoad v $ mkPtr id
                      modify (\s -> s { validRegisters = Map.insert id v (validRegisters s) })
                      return $ LLVMValueIdent v
        Just v -> return $ LLVMValueIdent v
    
-- Translate statement into LLVM statements.
translateStmt :: Stmt -> LLVMMonad ()
translateStmt (SAss id e) = do
    v <- translateExp e
    p <- getPtr id
    addInstruction $ LLVMStore v p
    modify (\s -> s { validRegisters = Map.delete id (validRegisters s) })
translateStmt (SExp e) = translateExp e >>= addInstruction . LLVMPrint

-- Translate program into LLVM statements.
translateProg :: Program -> LLVMMonad LLVMCode
translateProg (Prog stmts) = mapM_ translateStmt stmts >> gets (LLVMCode . reverse . producedInstructions)

execLLVMTranslate :: Program -> Either String LLVMCode
execLLVMTranslate prog = evalState (runExceptT $ translateProg prog) initLLVMState