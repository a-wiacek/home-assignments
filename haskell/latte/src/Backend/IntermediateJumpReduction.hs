module Backend.IntermediateJumpReduction(intermediateJumpReduction) where
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as Map
import Data.Maybe(mapMaybe)
import qualified Data.Set as Set

import Backend.Bimap
import Backend.ControlFlowGraph
import Backend.LLVMData

{-
This file attempts to correct unnecessary jumps introduced while generating code
for complex logical conditions. The schema is as follows:

There exists a block T with such that:
  * it has two predecessors Pred_1 and Pred_2
  * Pred_1 ends with instruction br cond1, Sib | T (by | I denote unspecified order of labels)
  * Pred_2 ends with br T
  * the only two instructions in T are:
    ~ cond2 = phi [LIT, Pred_1] [val, Pred_2], where LIT is literal (True / False)
        and val is a boolean value computed in Pred_2
    ~ br cond2, Succ_1 | Succ_2

We can modify these blocks:
  * jump in Pred_1 is now br cond1, Sib | Succ_x (x depends on value of LIT)
  * jump in Pred_2 is now br val Succ_1 | Succ_2
  * block T is deleted, since cond2 should not appear anywhere else in function code
  * phi instructions in Succ_1 and Succ_2 should not depend on T
-}

data SchemaEvidence = SchemaEvidence
    { predDecisive :: LlvmIdent
    , predNondecisive :: LlvmIdent
    , jumpLiteral :: Bool
    , jumpValue :: LlvmValue
    , succTrue :: LlvmIdent
    , succFalse :: LlvmIdent
    }

data IjrState = IjrState
    { blocks :: Map.Map LlvmIdent LlvmBlock
    , ijrCfg :: ControlFlowGraph
    }

findConstAndPrev :: (LlvmValue, LlvmIdent) -> (LlvmValue, LlvmIdent) -> Maybe (Bool, LlvmValue)
findConstAndPrev (val1, pred1) (val2, pred2) = case (val1, val2) of
    (LlvmValueBConst b, v) -> Just (b, v)
    (v, LlvmValueBConst b) -> Just (b, v)
    _ -> Nothing

blockMatchesInstructions :: LlvmBlock -> Maybe (Bool, LlvmValue, LlvmIdent, LlvmIdent)
blockMatchesInstructions (LlvmBlock _ instrs) = case instrs of
    [LlvmAssign i1 (LlvmPhi _ [opt1, opt2]), LlvmBrIf (LlvmValueIdent i2) succTrue succFalse]
        -> case findConstAndPrev opt1 opt2 of
            Nothing -> Nothing
            Just (b, val) -> if i1 == i2
                then Just (b, val, succTrue, succFalse)
                else Nothing
    _ -> Nothing

blockMatchesSchema :: LlvmIdent -> State IjrState (Maybe SchemaEvidence)
blockMatchesSchema ident = runMaybeT $ do
    (jumpLiteral, jumpValue, succTrue, succFalse) <- MaybeT $ gets (blockMatchesInstructions . (Map.! ident) . blocks)
    [pred1, pred2] <- gets (Set.toList . getPredecessors ident . ijrCfg)
    pred1succs <- gets (Set.toList . getSuccessors pred1 . ijrCfg)
    pred2succs <- gets (Set.toList . getSuccessors pred2 . ijrCfg)
    case (pred1succs, pred2succs) of
        ([_, _], [_]) -> return $ SchemaEvidence pred1 pred2 jumpLiteral jumpValue succTrue succFalse
        ([_], [_, _]) -> return $ SchemaEvidence pred2 pred1 jumpLiteral jumpValue succTrue succFalse
        _ -> MaybeT (return Nothing)

investigateBlock :: LlvmIdent -> State IjrState ()
investigateBlock ident = blockMatchesSchema ident >>= maybe (return ()) (changeBlock ident)

changeBlock :: LlvmIdent -> SchemaEvidence -> State IjrState ()
changeBlock ident evidence = do
    let pred1 = predDecisive evidence
    let pred2 = predNondecisive evidence
    let succ' = (if jumpLiteral evidence then succTrue else succFalse) evidence
    let succs = [succTrue evidence, succFalse evidence]
    modify $ \s -> s { blocks = Map.delete ident (blocks s)
                     , ijrCfg = addSuccessor pred1 succ'
                              $ flip (foldr (`addPredecessor` pred2)) succs
                              $ flip (foldr (addSuccessor pred2)) succs
                              $ removeBlock ident
                              $ ijrCfg s }
    pred1block <- gets ((Map.! pred1) . blocks)
    let LlvmBrIf (LlvmValueIdent cond1) dest1 dest2 = last (blockInstructions pred1block)
    let dest1' = if dest1 == ident then succ' else dest1
    let dest2' = if dest2 == ident then succ' else dest2
    let newJump1 = LlvmBrIf (LlvmValueIdent cond1) dest1' dest2'
    let newPred1block = modifyLastInstr (const newJump1) pred1block
    modify $ \s -> s { blocks = Map.insert pred1 newPred1block (blocks s) }
    let newJump2 = LlvmBrIf (jumpValue evidence) (succTrue evidence) (succFalse evidence)
    modify $ \s -> s { blocks = Map.adjust (modifyLastInstr $ const newJump2) pred2 (blocks s) }

runIje :: State IjrState ()
runIje = do
    blocks1 <- gets blocks
    forM_ (Map.keys blocks1) investigateBlock
    blocks2 <- gets blocks
    unless (blocks1 == blocks2) runIje

reduceIntermediateJumps :: LlvmFunction -> LlvmFunction
reduceIntermediateJumps fun = LlvmFunction (funSign fun) (blockMapToBlocks newBlocks)
    where cfg = buildControlFlowGraph fun
          bmap = funToBlockMap fun
          initState = IjrState bmap cfg
          newBlocks = blocks $ execState runIje initState

intermediateJumpReduction :: LlvmProgram -> LlvmProgram
intermediateJumpReduction (LlvmProgram decls tables consts funs)
    = LlvmProgram decls tables consts (map reduceIntermediateJumps funs)