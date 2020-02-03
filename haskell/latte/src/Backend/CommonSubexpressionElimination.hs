module Backend.CommonSubexpressionElimination(commonSubexpressionElimination) where
import Control.Monad
import Control.Monad.State.Strict
import Data.List(sort)
import Data.Maybe(catMaybes)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Backend.Bimap
import Backend.ControlFlowGraph
import Backend.DominationTree
import Backend.LLVMData

canonicalExpForm :: LlvmExp -> LlvmExp
canonicalExpForm (LlvmAdd val1 val2) = LlvmAdd (min val1 val2) (max val1 val2)
canonicalExpForm (LlvmMul val1 val2) = LlvmMul (min val1 val2) (max val1 val2)
canonicalExpForm (LlvmPhi t vals) =  LlvmPhi t (sort vals)
canonicalExpForm e = e

data CseState = CseState
    { blocks :: Map.Map LlvmIdent LlvmBlock
    , cseCfg :: ControlFlowGraph
    , expsInBlock :: Map.Map LlvmIdent (Map.Map LlvmExp LlvmIdent)
    , dominatorSets :: Map.Map LlvmIdent (Set.Set LlvmIdent)
    , thisBimap :: Bimap
    } deriving (Eq, Show)

findExpInDominators :: LlvmIdent -> [LlvmIdent] -> LlvmExp -> State CseState (Maybe LlvmIdent)
findExpInDominators _ [] _ = return Nothing
findExpInDominators ident (label:labels) exp = do
    exps <- gets ((Map.! label) . expsInBlock)
    let inRest = findExpInDominators ident labels exp
    maybe inRest (\oldIdent -> if ident == oldIdent then inRest else return (Just oldIdent)) (Map.lookup exp exps)

mapInstr :: LlvmIdent -> LlvmInstr -> State CseState (Maybe LlvmInstr)
mapInstr label instr' = do
    bimap <- gets thisBimap
    let instr = replaceValuesInstr bimap instr'
    case instr of
        LlvmAssign ident exp | isPureExp exp -> do
            dominators <- gets (Set.toList . (Map.! label) . dominatorSets)
            findExpInDominators ident dominators exp >>= maybe
                (do modify $ \s -> s { expsInBlock = Map.adjust (Map.insert exp ident) label (expsInBlock s) }
                    return $ Just instr
                )
                (\oldIdent -> do
                    modify $ \s -> s { thisBimap = insertForward (LlvmValueIdent ident)
                                                                 (LlvmValueIdent oldIdent)
                                                                 (thisBimap s) }
                    return Nothing
                )
        _ -> return $ Just instr

mapInBlock :: LlvmIdent -> State CseState ()
mapInBlock label = do
    LlvmBlock _ instrs <- gets ((Map.! label) . blocks)
    newBlock <- LlvmBlock label . catMaybes <$> mapM (mapInstr label) instrs
    modify $ \s -> s { blocks = Map.insert label newBlock (blocks s) }

runCse :: State CseState ()
runCse = do
    state1 <- get
    gets (Map.keys . blocks) >>= mapM_ mapInBlock
    state2 <- get
    unless (state1 == state2) runCse

eliminateCommonSubexpressions :: LlvmFunction -> LlvmFunction
eliminateCommonSubexpressions fun = LlvmFunction (funSign fun) (blockMapToBlocks newBlocks)
    where cfg = buildControlFlowGraph fun
          bmap = funToBlockMap fun
          initState = CseState bmap cfg (Map.map (const Map.empty) bmap) (buildDominatorSets fun) emptyBimap
          newBlocks = blocks $ execState runCse initState

commonSubexpressionElimination :: LlvmProgram -> LlvmProgram
commonSubexpressionElimination (LlvmProgram decls tables consts funs)
    = LlvmProgram decls tables consts (map eliminateCommonSubexpressions funs)