module Frontend.FunctionSignatures
    ( collectFunctionSignatures
    , FunctionSignatures
    , functionNames
    ) where
import Control.Monad(foldM)
import qualified Data.Map.Strict as Map

import Frontend.ClassHierarchy
import AbsLatte

-- The purpose of this file is to collect all function headers and check that:
--  * no two functions with the same name are defined
--  * function main has signature FunType IntType []

type FunctionSignatures = Map.Map String (FunType Pos)

initSignatures :: FunctionSignatures
initSignatures = Map.fromList
    [ ("printInt", printIntType)
    , ("printString", printStringType)
    , ("error", errorType)
    , ("readInt", readIntType)
    , ("readString", readStringType)
    ]

functionNames :: FunctionSignatures -> [String]
functionNames = Map.keys

collectFunctionSignatures :: Program Pos -> ClassHierarchy -> Either String FunctionSignatures
collectFunctionSignatures (Program _ defs) hierarchy = foldM analyzeDef initSignatures defs >>= postFold where
    analyzeDef signatures (TopFunDef _ fun@FunDef{})
        | Map.member (name fun) signatures = Left $ "Declaring function " ++ name fun ++ " for the second time" ++ loc
        | otherwise = case functionSignature hierarchy fun of
            Left err -> Left $ "Errors found in signature of function " ++ name fun ++ ":\n" ++ err
            Right sig -> Right $ Map.insert (name fun) sig signatures
        where loc = " at " ++ show (position fun)
    analyzeDef signatures _ = Right signatures
    postFold signatures = case Map.lookup "main" signatures of
        Nothing -> Left "Function \"main\" was not found"
        Just sign -> if sign == mainType
            then Right signatures
            else Left $ "Function \"main\" has wrong type: it should take no arguments and return int"
                     ++ ", but it has type " ++ name sign