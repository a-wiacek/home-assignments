module Backend.DeadBlocksElimination(deadBlocksElimination) where
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Backend.LLVMData

data DfsState = DfsState
    { visited :: Set.Set LlvmIdent
    , successors :: Map.Map LlvmIdent [LlvmIdent]
    }

addVertex :: LlvmIdent -> State DfsState ()
addVertex ident = modify $ \s -> s { visited = Set.insert ident (visited s) }
isVisited :: LlvmIdent -> State DfsState Bool
isVisited ident = gets (Set.member ident . visited)
getSuccessors :: LlvmIdent -> State DfsState [LlvmIdent]
getSuccessors ident = gets ((Map.! ident) . successors)

dfs :: LlvmIdent -> State DfsState ()
dfs ident = do
    b <- isVisited ident
    unless b $ addVertex ident >> getSuccessors ident >>= mapM_ dfs

removeDeadBlocks :: LlvmFunction -> LlvmFunction
removeDeadBlocks (LlvmFunction sign blocks) =
    let reachable = visited $ execState (dfs entryIdent) $ DfsState Set.empty
                            $ Map.fromList [(blockLabel b, computeSuccessors b) | b <- blocks]
    in LlvmFunction sign $ filter (flip Set.member reachable . blockLabel) blocks

-- Note: code produced by execLLVMTranslate might not be valid, but it will be valid for sure
-- after removing dead blocks.

deadBlocksElimination :: LlvmProgram -> LlvmProgram
deadBlocksElimination (LlvmProgram decls tables consts funs)
    = LlvmProgram decls tables consts (map removeDeadBlocks funs)