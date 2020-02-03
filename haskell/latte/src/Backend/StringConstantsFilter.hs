module Backend.StringConstantsFilter(removeUnusedStringConstants) where
import Data.List(isPrefixOf)
import qualified Data.Set as Set

import Backend.LLVMData

isUsedConstant :: Set.Set Int -> LlvmStringConstant -> Bool
isUsedConstant ns (LlvmStringConstant _ n) = n `Set.member` ns

extractConst :: LlvmValue -> Set.Set Int
extractConst (LlvmValueIdent (LlvmIdent s True)) = if "str." `isPrefixOf` s
    then Set.singleton $ read $ drop 4 s
    else Set.empty
extractConst _ = Set.empty

identsUsed :: LlvmInstr -> Set.Set Int
identsUsed (LlvmRet mret) = maybe Set.empty (extractConst . snd) mret
identsUsed (LlvmBr _) = Set.empty
identsUsed (LlvmBrIf val _ _) = extractConst val
identsUsed (LlvmStore t val1 val2) = extractConst val1 <> extractConst val2
identsUsed (LlvmAssign _ exp) = case exp of
    LlvmNeg val -> extractConst val
    LlvmAdd val1 val2 -> extractConst val1 <> extractConst val2
    LlvmSub val1 val2 -> extractConst val1 <> extractConst val2
    LlvmMul val1 val2 -> extractConst val1 <> extractConst val2
    LlvmDiv val1 val2 -> extractConst val1 <> extractConst val2
    LlvmMod val1 val2 -> extractConst val1 <> extractConst val2
    LlvmShl val _ -> extractConst val
    LlvmShr val _ -> extractConst val
    LlvmAnd val1 val2 -> extractConst val1 <> extractConst val2
    LlvmAlloca _ -> Set.empty
    LlvmLoad _ val1 -> extractConst val1
    LlvmCmp _ _ val1 val2 -> extractConst val1 <> extractConst val2
    LlvmCall _ f args -> foldr ((<>) . extractConst . llvmArgName) (extractConst f) args
    LlvmPhi _ choices -> foldr ((<>) . extractConst . fst) Set.empty choices
    LlvmGetElementPtr _ _ val refs -> foldr ((<>) . extractConst . snd) (extractConst val) refs
    LlvmBitcast _ val1 _ -> extractConst val1
    LlvmInsertValue _ val1 _ val2 _ -> extractConst val1 <> extractConst val2
    LlvmExtractValue _ _ val1 _ -> extractConst val1

collectBlock :: LlvmBlock -> Set.Set Int
collectBlock = mconcat . map identsUsed . blockInstructions
collectFunction :: LlvmFunction -> Set.Set Int
collectFunction = mconcat . map collectBlock . funBlocks
collectProgram :: LlvmProgram -> Set.Set Int
collectProgram = mconcat . map collectFunction . functions

removeUnusedStringConstants :: LlvmProgram -> LlvmProgram
removeUnusedStringConstants prog = prog
    { llvmStringConstants = filter (isUsedConstant $ collectProgram prog) (llvmStringConstants prog) }