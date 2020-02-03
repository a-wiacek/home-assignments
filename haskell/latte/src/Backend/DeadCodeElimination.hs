module Backend.DeadCodeElimination(deadCodeElimination) where
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Backend.ControlFlowGraph
import Backend.LLVMData

extractIdent :: LlvmValue -> Set.Set LlvmIdent
extractIdent (LlvmValueIdent ident) = Set.singleton ident
extractIdent _ = Set.empty

identsUsed :: LlvmInstr -> Set.Set LlvmIdent
identsUsed (LlvmRet mret) = maybe Set.empty (extractIdent . snd) mret
identsUsed (LlvmBr _) = Set.empty
identsUsed (LlvmBrIf val _ _) = extractIdent val
identsUsed (LlvmStore t val1 val2) = extractIdent val1 <> extractIdent val2
identsUsed (LlvmAssign _ exp) = case exp of
    LlvmNeg val -> extractIdent val
    LlvmAdd val1 val2 -> extractIdent val1 <> extractIdent val2
    LlvmSub val1 val2 -> extractIdent val1 <> extractIdent val2
    LlvmMul val1 val2 -> extractIdent val1 <> extractIdent val2
    LlvmDiv val1 val2 -> extractIdent val1 <> extractIdent val2
    LlvmMod val1 val2 -> extractIdent val1 <> extractIdent val2
    LlvmShl val1 _ -> extractIdent val1
    LlvmShr val1 _ -> extractIdent val1
    LlvmAnd val1 val2 -> extractIdent val1 <> extractIdent val2
    LlvmAlloca _ -> Set.empty
    LlvmLoad _ val1 -> extractIdent val1
    LlvmCmp _ _ val1 val2 -> extractIdent val1 <> extractIdent val2
    LlvmCall _ f args -> foldr ((<>) . extractIdent . llvmArgName) (extractIdent f) args
    LlvmPhi _ choices -> foldr ((<>) . extractIdent . fst) Set.empty choices
    LlvmGetElementPtr _ _ val refs -> foldr ((<>) . extractIdent . snd) (extractIdent val) refs
    LlvmBitcast _ val1 _ -> extractIdent val1
    LlvmInsertValue _ val1 _ val2 _ -> extractIdent val1 <> extractIdent val2
    LlvmExtractValue _ _ val1 _ -> extractIdent val1

identsUsedBlock :: LlvmBlock -> Set.Set LlvmIdent -> Set.Set LlvmIdent
identsUsedBlock block usedBySuccs = foldr ((<>) . identsUsed) usedBySuccs (blockInstructions block)

isInstrAlive :: Set.Set LlvmIdent -> LlvmInstr -> Bool
isInstrAlive usedByInstr instr = case instr of
    LlvmAssign ident exp -> not (isPureExp exp) || ident `Set.member` usedByInstr
    _ -> True

data DceState = DceState
    { blocks :: Map.Map LlvmIdent LlvmBlock
    , dceCfg :: ControlFlowGraph
    , usedInBlock :: Map.Map LlvmIdent (Set.Set LlvmIdent)
    }

updateUsedVariables :: LlvmIdent -> State DceState ()
updateUsedVariables ident = do
    succs <- gets (Set.toList . getSuccessors ident . dceCfg)
    usedBySuccs <- mconcat <$> forM succs (\l -> gets ((Map.! l) . usedInBlock))
    thisBlock <- gets ((Map.! ident) . blocks)
    let usedByIdent = identsUsedBlock thisBlock usedBySuccs
    modify $ \s -> s { usedInBlock = Map.insert ident usedByIdent (usedInBlock s) }

reachFixPoint :: State DceState ()
reachFixPoint = do
    usedInBlock1 <- gets usedInBlock
    gets (Map.keys . blocks) >>= mapM_ updateUsedVariables
    usedInBlock2 <- gets usedInBlock
    unless (usedInBlock1 == usedInBlock2) reachFixPoint

removeDeadInstrs :: LlvmIdent -> State DceState ()
removeDeadInstrs ident = do
    usedHere <- gets ((Map.! ident) . usedInBlock)
    instrs <- gets (filter (isInstrAlive usedHere) . blockInstructions . (Map.! ident) . blocks)
    modify $ \s -> s { blocks = Map.insert ident (LlvmBlock ident instrs) (blocks s) }

runDce :: State DceState ()
runDce = reachFixPoint >> gets (Map.keys . blocks) >>= mapM_ removeDeadInstrs

eliminateDeadCode :: LlvmFunction -> LlvmFunction
eliminateDeadCode fun = LlvmFunction (funSign fun) (blockMapToBlocks newBlocks)
    where cfg = buildControlFlowGraph fun
          bmap = funToBlockMap fun
          newBlocks = blocks $ execState runDce $ DceState bmap cfg $ Map.map (const Set.empty) bmap

deadCodeElimination :: LlvmProgram -> LlvmProgram
deadCodeElimination (LlvmProgram decls tables consts funs)
    = LlvmProgram decls tables consts (map eliminateDeadCode funs)