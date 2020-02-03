module LatteCompiler(translateProgram) where
import Control.Monad((>=>))

import AbsLatte
import Backend.ClassDataLayout(buildClassesDataLayout)
import Backend.CommonSubexpressionElimination(commonSubexpressionElimination)
import Backend.ConstantsOptimization(constantsOptimization)
import Backend.DeadBlocksElimination(deadBlocksElimination)
import Backend.DeadCodeElimination(deadCodeElimination)
import Backend.IntermediateJumpReduction(intermediateJumpReduction)
import Backend.LLVMData(LlvmProgram)
import Backend.LLVMTranslator(execLlvmTranslate)
import Backend.Mem2Reg(mem2Reg)
import Backend.StringConstantsFilter(removeUnusedStringConstants)
import Backend.TrivialPhiElimination(trivialPhiElimination)
import Backend.TrivialJumpElimination(trivialJumpsElimination)
import Frontend.ClassHierarchy(buildClassHierarchy)
import Frontend.ClassAnalysis(analyzeClasses)
import Frontend.FunctionSignatures(collectFunctionSignatures)
import Frontend.StringFix(fixStrings)
import Frontend.Typecheck(typecheck)
import Frontend.ConstantPropagation(propagateConstants)
import Frontend.DeadCodeElimination(eliminateDeadCode)
import Frontend.ReturnChecker(checkReturns)

-- Frontend optimizations can also fail, detecting errors
-- that would not be detected by typechecking (i. e. division by zero)
type FO = Program Pos -> Either String (Program Pos)

frontendOptimizations :: [FO]
frontendOptimizations = [propagateConstants, eliminateDeadCode]

iterFixM :: FO -> FO
iterFixM f x = f x >>= \y -> if x == y then return x else iterFixM f y

optimizeFrontendCode :: FO
optimizeFrontendCode = iterFixM (foldr (>=>) return frontendOptimizations)

type BO = LlvmProgram -> LlvmProgram

backendOptimizations :: [BO]
backendOptimizations =
    [ deadBlocksElimination
    , constantsOptimization
    , trivialJumpsElimination
    , intermediateJumpReduction
    , commonSubexpressionElimination
    , trivialPhiElimination
    , deadCodeElimination
    ]

iterFix :: BO -> BO
iterFix f x = let y = f x in if x == y then y else iterFix f y

optimizeBackendCode :: BO
optimizeBackendCode = iterFix (foldr (.) id backendOptimizations)

translateProgram :: Program Pos -> Either String LlvmProgram
translateProgram program' = let program = fixStrings program' in do
    -- FRONT END
    (hierarchy, order) <- buildClassHierarchy program
    classesInfo <- analyzeClasses program hierarchy order
    signatures <- collectFunctionSignatures program hierarchy
    typecheck program hierarchy classesInfo signatures
    -- At this point code is typed correctly. Some functions may modify
    -- code or reject it, but it is safe now to modify AST under that assumption.
    program <- optimizeFrontendCode program
    checkReturns program
    -- BACK END
    return $ removeUnusedStringConstants $ optimizeBackendCode $ mem2Reg $ deadBlocksElimination
           $ execLlvmTranslate signatures (buildClassesDataLayout program hierarchy order) program
        
    
    