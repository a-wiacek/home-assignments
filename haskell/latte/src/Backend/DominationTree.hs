module Backend.DominationTree(buildDominatorSets) where
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Backend.ControlFlowGraph
import Backend.LLVMData

data DominationState = DominationState
    { dominatorSets :: Map.Map LlvmIdent (Set.Set LlvmIdent)
    , dsCfg :: ControlFlowGraph
    , domLabels :: [LlvmIdent]
    }

updateDominatorSet :: LlvmIdent -> State DominationState ()
updateDominatorSet label = do
    preds <- gets (Set.toList . getPredecessors label . dsCfg)
    newPreds <- Set.union (Set.singleton label)
              . foldr1 Set.intersection
            <$> forM preds (\p -> gets ((Map.! p) . dominatorSets))
    modify $ \s -> s { dominatorSets = Map.insert label newPreds (dominatorSets s) }

runDominatorSets :: State DominationState ()
runDominatorSets = do
    dSets1 <- gets dominatorSets
    gets domLabels >>= mapM_ updateDominatorSet
    dSets2 <- gets dominatorSets
    unless (dSets1 == dSets2) runDominatorSets

buildDominatorSets :: LlvmFunction -> Map.Map LlvmIdent (Set.Set LlvmIdent)
buildDominatorSets fun = dominatorSets $ execState runDominatorSets initState
    where cfg = buildControlFlowGraph fun
          blockMap = funToBlockMap fun
          labels = Map.keysSet blockMap
          initDoms = Map.insert entryIdent (Set.singleton entryIdent)
                   $ Map.map (const labels) blockMap
          initState = DominationState initDoms cfg (Set.toList $ Set.delete entryIdent labels)