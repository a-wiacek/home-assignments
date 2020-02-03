{-# LANGUAGE LambdaCase #-}
module Backend.Mem2Reg(mem2Reg) where
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Backend.Bimap
import Backend.ControlFlowGraph
import Backend.LLVMData

-- Rewriting instructions to use phi nodes instead of allocas/stores
-- is done in two phases:
--  * the first phase replaces multiple loads and stores in each block separately,
--    leaving for each pointer at most one load at the beginning of the block
--    and at most one store at the end of the block
--  * the second phase rewrites loads/stores to phi
-- Notes: this concerns only local variables, loads/stores related to class fields
--    are not investigated

mem2Reg :: LlvmProgram -> LlvmProgram
mem2Reg (LlvmProgram decls tables consts funs)
    = LlvmProgram decls tables consts (map (phaseTwo . phaseOne) funs)

-----------------------------------------------------------

data DeconstructedBlock = DeconstructedBlock
    { phiInstrs :: [LlvmInstr]
    , loads :: Map.Map LlvmValue (LlvmValue, LlvmType) -- ptr |-> (val, t) ~ val = load t, t* ptr
    , middleInstrs :: [LlvmInstr]
    , stores :: Map.Map LlvmValue (LlvmValue, LlvmType) -- ptr |-> (val, t) ~ store t val, t* ptr
    , jumpInstr :: LlvmInstr
    }
initBlock = DeconstructedBlock
    { phiInstrs = []
    , loads = Map.empty
    , middleInstrs = []
    , stores = Map.empty
    , jumpInstr = error "no jump instr yet"
    }
revBlock :: DeconstructedBlock -> DeconstructedBlock
revBlock b = b { phiInstrs = reverse $ phiInstrs b
               , middleInstrs = reverse $ middleInstrs b
               }
replaceValuesDecBlock :: Bimap -> DeconstructedBlock -> DeconstructedBlock
replaceValuesDecBlock bimap (DeconstructedBlock phis ls mids ss jump)
    = DeconstructedBlock (map f phis) (g ls) (map f mids) (g ss) (f jump)
    where f = replaceValuesInstr bimap
          g = Map.map (\(val, t) -> (replaceValue bimap val, t))

data LastInstr = LLoad LlvmValue | LStore LlvmValue deriving (Eq, Ord)
data DeconstructionState = DeconstructionState
    { deconstructedBlocks :: Map.Map LlvmIdent DeconstructedBlock
    , lastInstr :: Map.Map LlvmValue LastInstr
    , thisBimap :: Bimap
    , currentBlockLabel :: LlvmIdent
    }
initState = DeconstructionState
    { deconstructedBlocks = Map.empty
    , lastInstr = Map.empty
    , thisBimap = emptyBimap
    , currentBlockLabel = error "not in block (phase one)"
    }

type SDM = State DeconstructionState
modifyThisBlock :: (DeconstructedBlock -> DeconstructedBlock) -> SDM ()
modifyThisBlock f = do
    label <- gets currentBlockLabel
    modify $ \s -> s { deconstructedBlocks = Map.adjust f label (deconstructedBlocks s) }
getLastInstr :: LlvmValue -> SDM (Maybe LastInstr)
getLastInstr ptr = gets (Map.lookup ptr . lastInstr)
setLastInstr :: LlvmValue -> LastInstr -> SDM ()
setLastInstr ptr instr = modify $ \s -> s { lastInstr = Map.insert ptr instr (lastInstr s) }
addReplacement :: LlvmValue -> LlvmValue -> SDM ()
addReplacement oldVal newVal = modify $ \s -> s { thisBimap = insertForward oldVal newVal (thisBimap s) }

deconstructSingleInstr :: LlvmInstr -> SDM ()
deconstructSingleInstr instr = case instr of
    LlvmRet{} -> addJump
    LlvmBr{} -> addJump
    LlvmBrIf{} -> addJump
    LlvmStore t val ptr -> if isPtr ptr
        then addStore ptr val t >> setLastInstr ptr (LStore val)
        else addInstr
    LlvmAssign ident exp -> case exp of
        LlvmLoad t ptr -> if isPtr ptr
            then let val = LlvmValueIdent ident in getLastInstr ptr >>= \case
                Just (LStore storeVal) -> addReplacement val storeVal
                Just (LLoad loadVal) -> addReplacement val loadVal
                Nothing -> addLoad ptr val t >> setLastInstr ptr (LLoad val)
            else addInstr
        LlvmPhi{} -> addPhi
        LlvmAlloca{} -> return ()
        _ -> addInstr
    where addJump = modifyThisBlock $ \b -> b { jumpInstr = instr }
          addInstr = modifyThisBlock $ \b -> b { middleInstrs = instr : middleInstrs b }
          addPhi = modifyThisBlock $ \b -> b { phiInstrs = instr : phiInstrs b }
          addStore ptr val t = modifyThisBlock $ \b -> b { stores = Map.insert ptr (val, t) (stores b) }
          addLoad ptr val t = modifyThisBlock $ \b -> b { loads = Map.insert ptr (val, t) (loads b) }


deconstructSingleBlock :: LlvmBlock -> SDM ()
deconstructSingleBlock (LlvmBlock label instrs) = do
    modify $ \s -> s
        { currentBlockLabel = label
        , lastInstr = Map.empty
        , deconstructedBlocks = Map.insert label initBlock (deconstructedBlocks s)
        , thisBimap = emptyBimap
        }
    forM_ instrs deconstructSingleInstr
    bimap <- gets thisBimap
    modify $ \s -> s { deconstructedBlocks = Map.adjust (replaceValuesDecBlock bimap . revBlock)
                                                        label (deconstructedBlocks s) }

runPhaseOne :: [LlvmBlock] -> SDM (Map.Map LlvmIdent DeconstructedBlock)
runPhaseOne blocks = forM_ blocks deconstructSingleBlock >> gets deconstructedBlocks

phaseOne :: LlvmFunction -> (LlvmFunSignature, Map.Map LlvmIdent DeconstructedBlock, ControlFlowGraph)
phaseOne fun@(LlvmFunction sign blocks)
    = (sign, evalState (runPhaseOne blocks) initState, buildControlFlowGraph fun)

-----------------------------------------------------------

getBlockTrivialPhis :: DeconstructedBlock -> [(LlvmValue, LlvmValue)]
getBlockTrivialPhis = foldr f [] . phiInstrs
    where f instr = case instr of
            LlvmAssign ident (LlvmPhi _ [(val, _)]) -> ((LlvmValueIdent ident, val):)
            _ -> id

getAllTrivialPhis :: Map.Map LlvmIdent DeconstructedBlock -> Bimap
getAllTrivialPhis = foldr (uncurry insertForward) emptyBimap
                  . Map.foldr (\b l -> getBlockTrivialPhis b ++ l) []

removeTrivialPhis :: DeconstructedBlock -> DeconstructedBlock
removeTrivialPhis b = b { phiInstrs = filter nontrivial (phiInstrs b) }
    where nontrivial instr = case instr of
            LlvmAssign ident (LlvmPhi _ [(val, _)]) -> False
            _ -> True

data PhiState = PhiState
    { funCfg :: ControlFlowGraph
    , decBlocks :: Map.Map LlvmIdent DeconstructedBlock
    , freshNameCounter :: Int
    }

type PM = State PhiState

getFreshIdent :: PM LlvmIdent
getFreshIdent = do
    c <- gets freshNameCounter
    modify $ \s -> s { freshNameCounter = freshNameCounter s + 1 }
    return $ LlvmIdent (".m2r." ++ show c) False

findValue :: LlvmValue -> LlvmType -> LlvmIdent -> PM LlvmValue
findValue ptr t label
      = gets (Map.lookup ptr . stores . (Map.! label) . decBlocks)
    >>= maybe (gets (Map.lookup ptr . loads . (Map.! label) . decBlocks) >>= maybe
                (getFreshIdent >>= mkPhi ptr label t)
                (\(LlvmValueIdent lassign, _) -> mkPhi ptr label t lassign)
              ) (return . fst)

mkPhi :: LlvmValue -> LlvmIdent -> LlvmType -> LlvmIdent -> PM LlvmValue
mkPhi ptr label t lassign = do
    let f b = b { loads = Map.delete ptr (loads b)
                , stores = Map.insertWith (const id) ptr (LlvmValueIdent lassign, t) (stores b) }
                -- preserve older value in stores if present
    modify $ \s -> s { decBlocks = Map.adjust f label (decBlocks s) }
    preds <- gets (Set.toList . getPredecessors label . funCfg)
    values <- forM preds (findValue ptr t)
    let g b = b { phiInstrs = LlvmAssign lassign (LlvmPhi t $ zip values preds) : phiInstrs b }
    modify $ \s -> s { decBlocks = Map.adjust g label (decBlocks s) }
    return $ LlvmValueIdent lassign

processBlock :: LlvmIdent -> PM ()
processBlock label
      = gets (Map.lookupMin . loads . (Map.! label) . decBlocks)
    >>= maybe (return ())
              (\(ptr, (LlvmValueIdent lassign, t)) -> mkPhi ptr label t lassign >> processBlock label)

processBlocks :: PM ()
processBlocks = do
    blocks <- gets (Map.keys . decBlocks)
    mapM_ processBlock blocks
    bimap <- gets (getAllTrivialPhis . decBlocks)
    modify $ \s -> s { decBlocks = Map.map (removeTrivialPhis . replaceValuesDecBlock bimap) (decBlocks s) }

rebuild :: LlvmIdent -> DeconstructedBlock -> LlvmBlock
rebuild label (DeconstructedBlock phis ls mids ss jump) = LlvmBlock label
     $ reverse phis
    ++ Map.elems (Map.mapWithKey mkLoad ls)
    ++ mids
    ++ [jump]
    where mkLoad ptr (LlvmValueIdent ident, t) = LlvmAssign ident (LlvmLoad t ptr)

phaseTwo :: (LlvmFunSignature, Map.Map LlvmIdent DeconstructedBlock, ControlFlowGraph) -> LlvmFunction
phaseTwo (sign, blocks, cfg) = LlvmFunction sign (blockMapToBlocks newBlocks)
    where newBlocks = Map.mapWithKey rebuild $ decBlocks $ execState processBlocks
                                             $ PhiState cfg blocks 0