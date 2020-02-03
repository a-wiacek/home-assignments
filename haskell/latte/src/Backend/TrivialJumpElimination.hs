module Backend.TrivialJumpElimination(trivialJumpsElimination) where
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Backend.ControlFlowGraph
import Backend.LLVMData

import Debug.Trace

-- If block A has only one succesor B and block B has only one succesor A,
-- we can glue blocks together.
-- Note that there are no phis at the beginning of B.

infixl 7 `glue`
glue :: LlvmBlock -> LlvmBlock -> LlvmBlock
glue (LlvmBlock l1 i1) (LlvmBlock l2 i2) = LlvmBlock l1 (init i1 ++ i2)

glueInGraph :: LlvmIdent -> LlvmIdent -> Set.Set LlvmIdent -> ControlFlowGraph -> ControlFlowGraph
glueInGraph l1 l2 succs2 (ControlFlowGraph succs preds) = ControlFlowGraph
    (Map.insert l1 (succs Map.! l2) $ Map.delete l2 succs)
    (Set.foldr (Map.adjust (Set.insert l1 . Set.delete l2)) (Map.delete l2 preds) succs2)

replaceInPhi :: LlvmIdent -> LlvmIdent -> LlvmBlock -> LlvmBlock
replaceInPhi oldIdent newIdent block = block { blockInstructions = g (blockInstructions block) }
    where f ident = if ident == oldIdent then newIdent else ident
          g (instr:instrs) = case instr of
            LlvmAssign ident (LlvmPhi t choices) -> LlvmAssign ident (LlvmPhi t (map (fmap f) choices)) : g instrs
            _ -> instr : instrs

data TjeState = TjeState
    { stateBlocks :: Map.Map LlvmIdent LlvmBlock
    , stateCfg :: ControlFlowGraph
    }

removeIfUnreachable :: LlvmIdent -> State TjeState ()
removeIfUnreachable ident = do
    preds <- gets (getPredecessors ident . stateCfg)
    when (Set.null preds) $
        modify $ \s -> s { stateBlocks = Map.delete ident (stateBlocks s)
                         , stateCfg = removeBlock ident (stateCfg s)
                         }

checkBlockGluing :: LlvmIdent -> State TjeState ()
checkBlockGluing ident = void $ runMaybeT $ do
    gets (Map.member ident . stateBlocks) >>= guard
    succs <- gets (getSuccessors ident . stateCfg)
    guard (Set.size succs == 1)
    let succ = Set.findMin succs
    preds <- gets (getPredecessors succ . stateCfg)
    guard (Set.size preds == 1 && Set.findMin preds == ident)
    TjeState blocks cfg <- get
    let succs2 = getSuccessors succ cfg
    let newBlocks = flip (foldr $ Map.adjust $ replaceInPhi succ ident) succs2
                  $ Map.insert ident (blocks Map.! ident `glue` blocks Map.! succ)
                  $ Map.delete succ blocks
    put $ TjeState newBlocks (glueInGraph ident succ succs2 cfg)

-- When the jump instruction is LlvmBrIf val ident1 ident2 and val is literal,
-- we can replace instruction with LlvmBr.

determinizeJump :: LlvmInstr -> Maybe (LlvmIdent, LlvmInstr)
determinizeJump (LlvmBrIf (LlvmValueBConst True) ident1 ident2) = Just (ident2, LlvmBr ident1)
determinizeJump (LlvmBrIf (LlvmValueBConst False) ident1 ident2) = Just (ident1, LlvmBr ident2)
determinizeJump jmp = Nothing

checkBlockDeterminizing :: LlvmIdent -> State TjeState ()
checkBlockDeterminizing ident = void $ runMaybeT $ do
    b <- MaybeT $ gets (Map.lookup ident . stateBlocks)
    (removedIdent, newJump) <- MaybeT $ return $ determinizeJump $ last $ blockInstructions b
    let newBlock = modifyLastInstr (const newJump) b
    modify $ \s -> s { stateBlocks = Map.insert ident newBlock (stateBlocks s)
                     , stateCfg = removeConnection ident removedIdent (stateCfg s)
                     }
    lift $ removeIfUnreachable removedIdent

-- When the only instruction in block is LlvmBr ident, we can replace jump destinations
-- in all predecessors.

blockMatchesInstructions :: LlvmBlock -> Maybe LlvmIdent
blockMatchesInstructions (LlvmBlock _ [LlvmBr ident]) = Just ident
blockMatchesInstructions _ = Nothing

replaceIdent :: LlvmIdent -> LlvmIdent -> LlvmIdent -> LlvmIdent 
replaceIdent oldIdent newIdent ident = if ident == oldIdent then newIdent else ident
replaceIdentsJump :: LlvmIdent -> LlvmIdent -> LlvmInstr -> LlvmInstr
replaceIdentsJump oldIdent newIdent instr = case instr of
    LlvmBr ident -> LlvmBr (f ident)
    LlvmBrIf val ident1 ident2 -> LlvmBrIf val (f ident1) (f ident2)
    where f = replaceIdent oldIdent newIdent

replaceInPhiMany :: LlvmIdent -> [LlvmIdent] -> LlvmBlock -> LlvmBlock
replaceInPhiMany ident idents block = block { blockInstructions = g (blockInstructions block) }
    where f = lookup ident . map (\(x, y) -> (y, x))
          g (instr:instrs) = case instr of
            LlvmAssign assIdent (LlvmPhi t choices) -> case f choices of
                Nothing -> instr : instrs
                Just val -> LlvmAssign assIdent (LlvmPhi t $ newChoices ++ oldChoices) : g instrs
                    where newChoices = [(val, nI) | nI <- idents]
                          oldChoices = filter (\(_, i) -> i /= ident) choices
            _ -> instr : instrs

checkBlockCutting :: LlvmIdent -> State TjeState ()
checkBlockCutting ident = void $ runMaybeT $ do
    guard (ident /= entryIdent)
    b <- MaybeT $ gets (Map.lookup ident . stateBlocks)
    dest <- MaybeT $ return $ blockMatchesInstructions b
    preds <- gets (Set.toList . getPredecessors ident . stateCfg)
    let f = modifyLastInstr $ replaceIdentsJump ident dest
    forM_ preds $ \pred -> modify (\s -> s { stateBlocks = Map.adjust f pred (stateBlocks s)
                                           , stateCfg = addSuccessor pred dest
                                                      $ addPredecessor dest pred
                                                      $ stateCfg s })
    let g = replaceInPhiMany ident preds
    modify $ \s -> s { stateBlocks = Map.adjust g dest $ Map.delete ident (stateBlocks s)
                     , stateCfg = removeBlock ident (stateCfg s)
                     }

-- When the last instruction is LlvmBrIf val ident1 ident2
-- and the only instruction in both ident1 and ident2 is returning boolean literal,
-- we can replace jump with return (and possibly negation).

justReturnsTrue, justReturnsFalse :: LlvmBlock -> Bool
justReturnsTrue (LlvmBlock _ [LlvmRet (Just (_, LlvmValueBConst True))]) = True
justReturnsTrue _ = False
justReturnsFalse (LlvmBlock _ [LlvmRet (Just (_, LlvmValueBConst False))]) = True
justReturnsFalse _ = False

checkBlockBooleanReturning :: LlvmIdent -> State TjeState ()
checkBlockBooleanReturning ident = void $ runMaybeT $ do
    b <- MaybeT $ gets (Map.lookup ident . stateBlocks)
    LlvmBrIf val ident1 ident2 <- return $ blockJumpInstr b
    succ1Block <- MaybeT $ gets (Map.lookup ident1 . stateBlocks)
    succ2Block <- MaybeT $ gets (Map.lookup ident2 . stateBlocks)
    let perform :: LlvmBlock -> MaybeT (State TjeState) ()
        perform newBlock = do
        modify $ \s -> s { stateBlocks = Map.insert ident newBlock (stateBlocks s)
                         , stateCfg = removeConnection ident ident1
                                    $ removeConnection ident ident2
                                    $ stateCfg s
                         }
        lift $ removeIfUnreachable ident1
        lift $ removeIfUnreachable ident2
    if justReturnsTrue succ1Block && justReturnsFalse succ2Block
        then perform $ modifyLastInstr (const $ LlvmRet $ Just (LlvmBool, val)) b
        else when (justReturnsFalse succ1Block && justReturnsTrue succ2Block) $
            let LlvmBlock _ instrs = b
                newIdent = LlvmIdent (unllvmIdent ident ++ ".j") False
                negAssign = LlvmAssign newIdent (LlvmNeg val)
                newRet = LlvmRet $ Just (LlvmBool, LlvmValueIdent newIdent)
                newBlock = LlvmBlock ident (init instrs ++ [negAssign, newRet])
            in perform newBlock
 
-- When the jump instruction is LlvmBrIf val ident1 ident1,
-- replace it with LlvmBr ident1.

removeIllusionOfChoice :: LlvmInstr -> LlvmInstr
removeIllusionOfChoice jmp@(LlvmBrIf _ ident1 ident2) = if ident1 == ident2 then LlvmBr ident1 else jmp
removeIllusionOfChoice instr = instr

runAction :: (LlvmIdent -> State TjeState ()) -> State TjeState ()
runAction action = gets (Map.keys . stateBlocks) >>= mapM_ action

checkBlocks :: State TjeState ()
checkBlocks = do
    blocks1 <- gets stateBlocks
    mapM_ runAction [checkBlockGluing, checkBlockCutting, checkBlockDeterminizing, checkBlockBooleanReturning]
    modify $ \s -> s { stateBlocks = Map.map (modifyLastInstr removeIllusionOfChoice) (stateBlocks s) }
    blocks2 <- gets stateBlocks
    unless (blocks1 == blocks2) checkBlocks

eliminateTrivialJumps :: LlvmFunction -> LlvmFunction
eliminateTrivialJumps fun = LlvmFunction (funSign fun) (blockMapToBlocks newBlocks)
    where cfg = buildControlFlowGraph fun
          newBlocks = stateBlocks $ execState checkBlocks $ TjeState (funToBlockMap fun) cfg

trivialJumpsElimination :: LlvmProgram -> LlvmProgram
trivialJumpsElimination (LlvmProgram decls tables consts funs)
    = LlvmProgram decls tables consts (map eliminateTrivialJumps funs)