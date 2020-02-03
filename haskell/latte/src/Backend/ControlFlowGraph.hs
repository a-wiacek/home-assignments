module Backend.ControlFlowGraph where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Backend.LLVMData

data ControlFlowGraph = ControlFlowGraph
    { successors :: Map.Map LlvmIdent (Set.Set LlvmIdent)
    , predecessors :: Map.Map LlvmIdent (Set.Set LlvmIdent)
    } deriving (Eq, Show)
initCfg = ControlFlowGraph Map.empty (Map.singleton entryIdent Set.empty)
getSuccessors, getPredecessors :: LlvmIdent -> ControlFlowGraph -> Set.Set LlvmIdent
getSuccessors ident cfg = successors cfg Map.! ident
getPredecessors ident cfg = predecessors cfg Map.! ident
removeBlock :: LlvmIdent -> ControlFlowGraph -> ControlFlowGraph
removeBlock ident cfg@(ControlFlowGraph successors predecessors) =
    let succs = getSuccessors ident cfg
        preds = getPredecessors ident cfg
        newSuccessors = Set.foldr (Map.adjust (Set.delete ident)) successors preds
        newPredecessors = Set.foldr (Map.adjust (Set.delete ident)) predecessors succs
    in ControlFlowGraph newSuccessors newPredecessors
addSuccessor, addPredecessor :: LlvmIdent -> LlvmIdent -> ControlFlowGraph -> ControlFlowGraph
addSuccessor ident succ cfg = cfg { successors = Map.adjust (Set.insert succ) ident (successors cfg) }
addPredecessor ident pred cfg = cfg { predecessors = Map.adjust (Set.insert pred) ident (predecessors cfg) }
removeConnection :: LlvmIdent -> LlvmIdent -> ControlFlowGraph -> ControlFlowGraph
removeConnection pred succ cfg = cfg { successors = Map.adjust (Set.delete succ) pred (successors cfg)
                                     , predecessors = Map.adjust (Set.delete pred) succ (predecessors cfg)
                                     }

addPred :: LlvmIdent -> LlvmIdent
        -> Map.Map LlvmIdent (Set.Set LlvmIdent)
        -> Map.Map LlvmIdent (Set.Set LlvmIdent)
addPred thisLabel prevLabel = Map.alter (Just . maybe (Set.singleton prevLabel) (Set.insert prevLabel)) thisLabel

lookAtOneBlock :: LlvmBlock -> ControlFlowGraph -> ControlFlowGraph
lookAtOneBlock (LlvmBlock thisLabel instrs) (ControlFlowGraph succs preds) = case last instrs of
    LlvmRet{} -> ControlFlowGraph (Map.insert thisLabel Set.empty succs) preds
    LlvmBr nextLabel -> ControlFlowGraph (Map.insert thisLabel (Set.singleton nextLabel) succs)
                                         (addPred nextLabel thisLabel preds)
    LlvmBrIf _ trueLabel falseLabel
        -> ControlFlowGraph (Map.insert thisLabel (Set.fromList [trueLabel, falseLabel]) succs)
                            (addPred trueLabel thisLabel $ addPred falseLabel thisLabel preds)
    i -> error $ "Block ends with " ++ show i ++ ", which is not a jump instruction"

buildControlFlowGraph :: LlvmFunction -> ControlFlowGraph
buildControlFlowGraph (LlvmFunction _ blocks) = foldr lookAtOneBlock initCfg blocks