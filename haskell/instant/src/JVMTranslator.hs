module JVMTranslator(execJVMTranslate) where
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import AbsInstant
import JVMData

data JVMState = JVMState
    { variablesLocalIndices :: Map.Map Ident Int -- Using undeclared variable will throw error and terminate translator.
    , producedInstructions :: [JVMInstr] -- For performance reasons, this is reversed list of generated instructions.
    }
initJVMState = JVMState Map.empty []

data JVMInstrInfo = JVMInstrInfo
    { stackUsage :: Int -- Analyze AST to minimize total stack size.
    , generatedInstructions :: [JVMInstr] -> [JVMInstr] -- Difference list to keep constant time concatenation.
    }

type JVMMonad a = ExceptT String (State JVMState) a

addInstructions :: [JVMInstr] -> JVMMonad ()
addInstructions i = modify (\s -> s { producedInstructions = i ++ producedInstructions s })

-- Get index of local variable, assigning it if necessary.
getLocalsIndex :: Bool -> Ident -> JVMMonad Int
getLocalsIndex declareWhenNew id@(Ident rawId) = do
    ix <- gets variablesLocalIndices
    case Map.lookup id ix of
        Nothing -> if declareWhenNew
            then let i = Map.size ix + 1
                 in modify (\s -> s { variablesLocalIndices = Map.insert id i ix }) >> return i
            else throwE $ "Aborting: Usage of undeclared variable " ++ rawId
        Just i -> return i

-- Translate expression into JVM statements, optimizing stack usage.
translateExp :: Exp -> JVMMonad JVMInstrInfo
translateExp (ExpAdd e1 e2) = do
    JVMInstrInfo usage1 instr1 <- translateExp e1
    JVMInstrInfo usage2 instr2 <- translateExp e2
    return $ JVMInstrInfo (1 + min usage1 usage2) $ (JVMAdd :) . if usage1 >= usage2
        then instr2 . instr1
        else instr1 . instr2
translateExp (ExpSub e1 e2) = do
    JVMInstrInfo usage1 instr1 <- translateExp e1
    JVMInstrInfo usage2 instr2 <- translateExp e2
    return $ JVMInstrInfo (1 + min usage1 usage2) $ (JVMSub :) . if usage1 >= usage2
        then instr2 . instr1
        else (JVMSwap :) . instr1 . instr2
translateExp (ExpMul e1 e2) = do
    JVMInstrInfo usage1 instr1 <- translateExp e1
    JVMInstrInfo usage2 instr2 <- translateExp e2
    return $ JVMInstrInfo (1 + min usage1 usage2) $ (JVMMul :) . if usage1 >= usage2
        then instr2 . instr1
        else instr1 . instr2
translateExp (ExpDiv e1 e2) = do
    JVMInstrInfo usage1 instr1 <- translateExp e1
    JVMInstrInfo usage2 instr2 <- translateExp e2
    return $ JVMInstrInfo (1 + min usage1 usage2) $ (JVMDiv :) . if usage1 >= usage2
        then instr2 . instr1
        else (JVMSwap :) . instr1 . instr2
translateExp (ExpLit n) = return $ JVMInstrInfo 1 (JVMConst (fromInteger n):)
translateExp (ExpVar id) = do
    p <- getLocalsIndex False id
    return $ JVMInstrInfo 1 (JVMLoad p :)
    
-- Translate statement into JVM statements.
translateStmt :: Stmt -> JVMMonad ()
translateStmt (SAss id e) = do
    ii <- translateExp e
    p <- getLocalsIndex True id
    addInstructions $ JVMStore p : generatedInstructions ii []
translateStmt (SExp e) = do
    ii <- translateExp e
    addInstructions [JVMGetPrintStream]
    addInstructions $ JVMPrint : generatedInstructions ii []

-- Translate program into JVM statements.
translateProg :: String -> Program -> JVMMonad JVMCode
translateProg className (Prog stmts) = mapM_ translateStmt stmts
                                    >> gets (JVMCode className . reverse . producedInstructions)

execJVMTranslate :: String -> Program -> Either String JVMCode
execJVMTranslate className prog = evalState (runExceptT $ translateProg className prog) initJVMState