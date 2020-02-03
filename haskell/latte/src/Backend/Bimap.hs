{-# LANGUAGE LambdaCase #-}
module Backend.Bimap where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Backend.LLVMData

data Bimap = Bimap
    { mapForward :: Map.Map LlvmValue LlvmValue
    , mapBackward :: Map.Map LlvmValue (Set.Set LlvmValue)
    } deriving (Eq, Show)
emptyBimap = Bimap Map.empty Map.empty

insertForward :: LlvmValue -> LlvmValue -> Bimap -> Bimap
insertForward oldVal newVal (Bimap forwardMap backwardMap) =
    case Map.lookup oldVal forwardMap of
        Just e -> error $ "trying to map element twice " ++ show e
        Nothing -> Bimap newForwardMap newBackwardMap where
            theVal = Map.findWithDefault newVal newVal forwardMap
            preimage = Set.insert oldVal $ Map.findWithDefault Set.empty oldVal backwardMap
            newForwardMap = Map.insert oldVal theVal
                          $ Set.foldr (`Map.insert` theVal) forwardMap preimage
            newBackwardMap = Map.insert oldVal Set.empty
                           $ Map.alter (Just . maybe preimage (Set.union preimage)) theVal backwardMap

replaceValuePrimitive :: LlvmValue -> LlvmValue -> LlvmValue -> LlvmValue
replaceValuePrimitive oldValue newValue thisValue
    | oldValue == thisValue = newValue
    | otherwise = thisValue

replaceValue :: Bimap -> LlvmValue -> LlvmValue
replaceValue bimap thisValue = Map.foldrWithKey replaceValuePrimitive thisValue (mapForward bimap)

replaceValuesExp :: Bimap -> LlvmExp -> LlvmExp
replaceValuesExp bimap = \case
    LlvmNeg val1 -> LlvmNeg (f val1)
    LlvmAdd val1 val2 -> LlvmAdd (f val1) (f val2)
    LlvmSub val1 val2 -> LlvmSub (f val1) (f val2)
    LlvmMul val1 val2 -> LlvmMul (f val1) (f val2)
    LlvmDiv val1 val2 -> LlvmDiv (f val1) (f val2)
    LlvmMod val1 val2 -> LlvmMod (f val1) (f val2)
    LlvmShl val1 n -> LlvmShl (f val1) n
    LlvmShr val1 n -> LlvmShr (f val1) n
    LlvmAnd val1 val2 -> LlvmAnd (f val1) (f val2)
    LlvmAlloca t -> LlvmAlloca t
    LlvmLoad t val1 -> LlvmLoad t (f val1)
    LlvmCmp op t val1 val2 -> LlvmCmp op t (f val1) (f val2)
    LlvmCall t val1 args -> LlvmCall t (f val1) (map replaceArg args)
    LlvmPhi t choices -> LlvmPhi t (map (\(val, i) -> (f val, i)) choices)
    LlvmGetElementPtr t1 t2 val1 vals -> LlvmGetElementPtr t1 t2 (f val1) (map (fmap f) vals)
    LlvmBitcast t1 val1 t2 -> LlvmBitcast t1 (f val1) t2
    LlvmInsertValue t1 val1 t2 val2 n -> LlvmInsertValue t1 (f val1) t2 (f val2) n
    LlvmExtractValue t1 t2 val1 n -> LlvmExtractValue t1 t2 (f val1) n
    where f = replaceValue bimap
          replaceArg (LlvmArg t val1) = LlvmArg t (f val1)

replaceValuesInstr :: Bimap -> LlvmInstr -> LlvmInstr
replaceValuesInstr bimap = \case
    LlvmRet ret -> LlvmRet (fmap (fmap f) ret)
    LlvmBr ident -> LlvmBr ident
    LlvmBrIf val1 ident1 ident2 -> LlvmBrIf (f val1) ident1 ident2
    LlvmStore t val1 val2 -> LlvmStore t (f val1) (f val2)
    LlvmAssign ident exp -> LlvmAssign ident (replaceValuesExp bimap exp)
    where f = replaceValue bimap

replaceValuesBlock :: Bimap -> LlvmBlock -> LlvmBlock
replaceValuesBlock bimap (LlvmBlock label instrs) = LlvmBlock label $ map (replaceValuesInstr bimap) instrs