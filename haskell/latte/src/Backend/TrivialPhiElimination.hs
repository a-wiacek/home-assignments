module Backend.TrivialPhiElimination(trivialPhiElimination) where
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Data.Maybe(isNothing)
import qualified Data.Set as Set

import Backend.Bimap
import Backend.ControlFlowGraph
import Backend.DominationTree
import Backend.LLVMData

data TpeState = TpeState
    { blocks :: Map.Map LlvmIdent LlvmBlock
    , thisBimap :: Bimap
    } deriving (Eq, Show)

trivialChoice :: [(LlvmValue, LlvmIdent)] -> Maybe LlvmValue
trivialChoice l@((val, _):_) = if all ((==val) . fst) l
    then Just val
    else Nothing

getBlockTrivialPhis :: LlvmBlock -> [(LlvmValue, LlvmValue)]
getBlockTrivialPhis = foldr f [] . blockInstructions
    where f instr = case instr of
            LlvmAssign ident (LlvmPhi _ vals) -> case trivialChoice vals of
                Just val -> ((LlvmValueIdent ident, val):)
                Nothing -> id
            _ -> id

filterNontrivial :: LlvmBlock -> LlvmBlock
filterNontrivial block = block { blockInstructions = filter f (blockInstructions block) }
    where f instr = case instr of
            LlvmAssign ident (LlvmPhi _ vals) -> isNothing (trivialChoice vals)
            _ -> True

collectTrivialPhis :: State TpeState ()
collectTrivialPhis = do
    oldBimap <- gets thisBimap
    newBimap <- gets ( foldr (uncurry insertForward) oldBimap
                     . Map.foldr (\block tps -> getBlockTrivialPhis block ++ tps) []
                     . blocks
                     )
    modify $ \s -> s { thisBimap = newBimap }

runTpe :: State TpeState ()
runTpe = do
    blocks1 <- gets blocks
    collectTrivialPhis
    bimap <- gets thisBimap
    modify $ \s -> s { blocks = Map.map (replaceValuesBlock bimap . filterNontrivial) (blocks s) }
    blocks2 <- gets blocks
    unless (blocks1 == blocks2) runTpe

eliminateTrivialPhis :: LlvmFunction -> LlvmFunction
eliminateTrivialPhis fun = LlvmFunction (funSign fun) (blockMapToBlocks newBlocks)
    where bmap = funToBlockMap fun
          initState = TpeState bmap emptyBimap
          newBlocks = blocks $ execState runTpe initState

trivialPhiElimination :: LlvmProgram -> LlvmProgram
trivialPhiElimination (LlvmProgram decls tables consts funs)
    = LlvmProgram decls tables consts (map eliminateTrivialPhis funs)
