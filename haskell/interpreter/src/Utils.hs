module Utils where

import qualified Data.Set as Set
import Control.Monad.Trans

import AbsGrammar
import MemoryContent


hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates l = (length l) /= (length $ Set.toList $ Set.fromList l)

compose :: [a -> a] -> a -> a
compose fs = foldl (flip (.)) id fs

prettyVarType :: VarType -> String
prettyVarType IntegerType = "Integer"
prettyVarType DoubleType = "Double"
prettyVarType StringType = "String"
prettyVarType BoolType = "Bool"

prettyExpType :: ExpType -> String
prettyExpType VoidType = "()"
prettyExpType (ExpType vt) = prettyVarType vt

justArgType :: FuncArg -> ExpType
justArgType (FuncArgVal t) = ExpType t
justArgType (FuncArgRef t) = ExpType t
justArgTypes :: FunctionInMemory -> [ExpType]
justArgTypes f = map justArgType $ funcArgs f
justVarType :: ExpType -> VarType
justVarType (ExpType x) = x

prettyPShow :: PIdent -> String
prettyPShow (PIdent ((x, y), s)) =
    s ++ " at line " ++ show x ++ ", column " ++ show y 

getFromPIdent :: PIdent -> String
getFromPIdent (PIdent (_, s)) = s

eConcat :: String -> [String] -> [String]
eConcat "" l = l
eConcat s l = s:l
