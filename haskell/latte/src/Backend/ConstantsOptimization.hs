module Backend.ConstantsOptimization(constantsOptimization) where
import Control.Monad
import Control.Monad.State.Strict
import Data.Bits
import Data.Int
import qualified Data.Map.Strict as Map
import Data.Maybe(catMaybes)
import qualified Data.Set as Set

import Backend.Bimap
import Backend.ControlFlowGraph
import Backend.LLVMData

{- List of actions
x = OP const, const -> if possible: compute x, replace it, remove instruction
x = add 0, y / add y, 0 -> replace x with y, remove instruction
x = add y, y -> replace with x = shl y, 1
x = sub y, 0 -> replace x with y, remove instruction
x = sub y, y -> replace x with 0, remove instruction
x = mul 0, y / mul y, 0 -> replace x with 0, remove instruction
x = mul 1, y / mul y, 1 -> replace x with y, remove instruction
x = mul (2^k), y / mul y, (2^k) -> replace with x = shl y, k
x = sdiv y, 1 -> replace x with y, remove instruction
x = sdiv y, (2^k) -> replace with x = ashr y, k
x = sdiv y, y -> ignored! y might be 0
x = srem y, 1 -> replace x with 0, remove instruction
x = srem y, (2^k) -> replace with x = and y, (2^k - 1)
x = srem y, y -> ignored! y might be 0
-}

exactLogPairs :: [(Int32, Int)]
exactLogPairs = [(2^x, x) | x <- [1..31]]
exactLog :: Int32 -> Maybe Int
exactLog n = lookup n exactLogPairs

data CoState = CoState
    { stateBlocks :: Map.Map LlvmIdent LlvmBlock
    , thisBimap :: Bimap
    } deriving (Eq, Show)

mapInstr :: LlvmInstr -> State CoState (Maybe LlvmInstr)
mapInstr instr' = do
    bimap <- gets thisBimap
    let instr = replaceValuesInstr bimap instr'
    let noChange :: State CoState (Maybe LlvmInstr)
        noChange = return (Just instr)
    case instr of
        LlvmAssign ident exp ->
            let replaceIdentWith :: LlvmValue -> State CoState (Maybe LlvmInstr)
                replaceIdentWith newValue = do
                    modify $ \s -> s { thisBimap = insertForward (LlvmValueIdent ident) newValue bimap }
                    return Nothing
                replaceExpWith :: LlvmExp -> State CoState (Maybe LlvmInstr)
                replaceExpWith = return . Just . LlvmAssign ident
            in case exp of
            LlvmAdd val1 val2 -> case (val1, val2) of
                (LlvmValueIConst n1, LlvmValueIConst n2) -> replaceIdentWith $ LlvmValueIConst (n1 + n2)
                (LlvmValueIConst 0, _) -> replaceIdentWith val2
                (_, LlvmValueIConst 0) -> replaceIdentWith val1
                _ -> if val1 == val2
                    then replaceExpWith $ LlvmShl val1 1
                    else noChange
            LlvmSub val1 val2 -> case (val1, val2) of
                (LlvmValueIConst n1, LlvmValueIConst n2) -> replaceIdentWith $ LlvmValueIConst (n1 - n2)
                (_, LlvmValueIConst 0) -> replaceIdentWith val1
                _ -> if val1 == val2
                    then replaceIdentWith $ LlvmValueIConst 0
                    else noChange
            LlvmMul val1 val2 -> case (val1, val2) of
                (LlvmValueIConst n1, LlvmValueIConst n2) -> replaceIdentWith $ LlvmValueIConst (n1 * n2)
                (LlvmValueIConst 0, _) -> replaceIdentWith $ LlvmValueIConst 0
                (_, LlvmValueIConst 0) -> replaceIdentWith $ LlvmValueIConst 0
                (LlvmValueIConst 1, _) -> replaceIdentWith val2
                (_, LlvmValueIConst 1) -> replaceIdentWith val1
                (LlvmValueIConst n, _) -> maybe noChange (replaceExpWith . LlvmShl val2) (exactLog n)
                (_, LlvmValueIConst n) -> maybe noChange (replaceExpWith . LlvmShl val1) (exactLog n)
                _ -> noChange
            LlvmDiv val1 val2 -> case (val1, val2) of
                (LlvmValueIConst n1, LlvmValueIConst n2) -> if n2 == 0
                    then noChange -- move error to runtime
                    else replaceIdentWith $ LlvmValueIConst (n1 `div` n2)
                (_, LlvmValueIConst 1) -> replaceIdentWith val1
                (_, LlvmValueIConst n) -> maybe noChange (replaceExpWith . LlvmShr val1) (exactLog n)
                _ -> noChange
            LlvmMod val1 val2 -> case (val1, val2) of
                (LlvmValueIConst n1, LlvmValueIConst n2) -> if n2 == 0
                    then noChange -- move error to runtime
                    else replaceIdentWith $ LlvmValueIConst (n1 `mod` n2)
                (_, LlvmValueIConst 1) -> replaceIdentWith $ LlvmValueIConst 0
                (_, LlvmValueIConst n) -> maybe noChange (const $ replaceExpWith $ LlvmAnd val1 $ LlvmValueIConst $ n - 1) (exactLog n)
                _ -> noChange
            LlvmShl val e -> case val of
                LlvmValueIConst n -> replaceIdentWith $ LlvmValueIConst $ shiftL n e
                _ -> noChange
            LlvmShr val e -> case val of
                LlvmValueIConst n -> replaceIdentWith $ LlvmValueIConst $ shiftR n e
                _ -> noChange
            LlvmAnd val1 val2 -> case (val1, val2) of
                (LlvmValueIConst n1, LlvmValueIConst n2) -> replaceIdentWith $ LlvmValueIConst $ n1 .&. n2
                _ -> noChange
            LlvmCmp cmpType t val1 val2 -> if val1 == val2
                then replaceIdentWith $ LlvmValueBConst $ cmpType `elem` [CmpEq, CmpSge, CmpSle]
                else case (val1, val2) of
                    (LlvmValueIConst n1, LlvmValueIConst n2) -> replaceIdentWith $ LlvmValueBConst $ llvmCmpTypeToOp cmpType n1 n2
                    _ -> noChange
            _ -> noChange
        _ -> noChange

mapInBlock :: LlvmIdent -> State CoState ()
mapInBlock label = do
    LlvmBlock _ instrs <- gets ((Map.! label) . stateBlocks)
    newBlock <- LlvmBlock label . catMaybes <$> mapM mapInstr instrs
    modify $ \s -> s { stateBlocks = Map.insert label newBlock (stateBlocks s) }

runCo :: State CoState ()
runCo = do
    state1 <- get
    gets (Map.keys . stateBlocks) >>= mapM_ mapInBlock
    state2 <- get
    unless (state1 == state2) runCo

simplifyExpressions :: LlvmFunction -> LlvmFunction
simplifyExpressions fun = LlvmFunction (funSign fun) (blockMapToBlocks newBlocks)
    where newBlocks = stateBlocks $ execState runCo $ CoState (funToBlockMap fun) emptyBimap

constantsOptimization :: LlvmProgram -> LlvmProgram
constantsOptimization (LlvmProgram decls tables consts funs)
    = LlvmProgram decls tables consts (map simplifyExpressions funs)