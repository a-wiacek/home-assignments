{-# LANGUAGE FlexibleInstances #-}
module Frontend.ClassHierarchy
    ( buildClassHierarchy
    , ClassHierarchy
    , classExists
    , isSubclassOf
    , isSuperclassOf
    , isValidAssignment
    , FunType
    , funTypeToLlvmSignature
    , mainType
    , printIntType
    , printStringType
    , errorType
    , readIntType
    , readStringType
    , retType
    , argTypes
    , getLlvmFunType
    , functionSignature
    ) where
import Control.Monad(foldM)
import Data.List(intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import AbsLatte
import Backend.LLVMData
import qualified Utils.ListSet as LS

-- The purpose of this file is to validate hierarchy of classes
-- without investigating their members, i. e. checking that:
--  * each class is declared only once,
--  * class does not extend nonexistent class,
--  * there are no cycles in inheritance chains.

data ClassHierarchy = ClassHierarchy
    { existingClasses :: Set.Set String
    , superclasses :: Map.Map String String
    }

emptyClassHierarchy :: ClassHierarchy
emptyClassHierarchy = ClassHierarchy Set.empty Map.empty

classExists :: String -> ClassHierarchy -> Bool
classExists className hierarchy = Set.member className (existingClasses hierarchy)

addBaseClass :: String -> ClassHierarchy -> ClassHierarchy
addBaseClass className hierarchy = hierarchy { existingClasses = Set.insert className (existingClasses hierarchy) }

addExtendedClass :: String -> String -> ClassHierarchy -> ClassHierarchy
addExtendedClass className superclassName hierarchy = ClassHierarchy
    { existingClasses = Set.insert className (existingClasses hierarchy)
    , superclasses = Map.insert className superclassName (superclasses hierarchy)
    }

-- Is class1 subclass of class2 in given hierarchy?
isSubclassOf :: ClassHierarchy -> String -> String -> Bool
isSubclassOf hierarchy class1 class2 = goUpwards class1 where
    goUpwards c = c == class2 || maybe False goUpwards (Map.lookup c (superclasses hierarchy))
    
isSuperclassOf :: ClassHierarchy -> String -> String -> Bool
isSuperclassOf = flip . isSubclassOf

-- Function signatures produced by functionSignature are valid:
--  * function has proper return type: basic type or existing class
--  * arguments have proper types: basic type (except for void) or existing class

data FunType a = FunType
    { funPosition :: a
    , retType :: Type a
    , argTypes :: [Type a]
    }

instance Eq (FunType a) where
    FunType _ q w == FunType _ a s = q == a && w == s

instance Description (FunType Pos) where
    name t = nameManyTypes (argTypes t) ++ " -> " ++ name (retType t)
    position = safePos . funPosition

isValidArgType :: ClassHierarchy -> Arg Pos -> Either String (Type Pos)
isValidArgType hierarchy (Arg _ t argIdent) = case t of
    VoidType _ -> Left $ errPrefix ++ "is void"
    ClassType{} -> if classExists className hierarchy
        then Right t
        else Left $ errPrefix ++ "(" ++ className ++ ") does not exist"
        where className = name t
    _ -> Right t
    where errPrefix = "Type of argument " ++ unIdent argIdent ++ " "

isValidRetType :: ClassHierarchy -> Type Pos -> Either String (Type Pos)
isValidRetType hierarchy t = case t of
    ClassType{} -> if classExists className hierarchy
        then Right t
        else Left $ "Return type " ++ className ++ " does not exist"
        where className = name t
    _ -> Right t

funTypeToLlvmSignature :: String -> FunType Pos -> LlvmFunSignature
funTypeToLlvmSignature funName (FunType _ retType argTypes)
    = LlvmFunSignature (LlvmIdent funName True)
                       (map typeToLlvmType argTypes)
                       (typeToLlvmType retType)

-- This function is intended to be used in backend, where types are checked to be correct!
getLlvmFunType :: String -> FunDef Pos -> LlvmType
getLlvmFunType className (FunDef _ retType _ args _)
    = LlvmFunType (typeToLlvmType retType) (LlvmPtr (LlvmClass className) : map (typeToLlvmType . argType) args)

functionSignature :: ClassHierarchy -> FunDef Pos -> Either String (FunType Pos)
functionSignature hierarchy (FunDef pos retType _ args _)
    = FunType pos <$> isValidRetType hierarchy retType <*> mapM (isValidArgType hierarchy) args

isValidAssignment :: ClassHierarchy -> Type Pos -> Type Pos -> Bool
isValidAssignment _ (VoidType _) _ = False -- should not happen, just in case
isValidAssignment _ _ (VoidType _) = False -- should not happen, just in case
isValidAssignment hierarchy (ClassType _ ident1) (ClassType _ ident2)
    = isSubclassOf hierarchy (unIdent ident1) (unIdent ident2)
isValidAssignment _ t1 t2 = t1 == t2

-- Main function shoud have such signature.

mainType :: FunType Pos
mainType = FunType Nothing intType []

-- Signatures of predefined functions.

printIntType, printStringType, errorType, readIntType, readStringType :: FunType Pos
printIntType = FunType Nothing voidType [intType]
printStringType = FunType Nothing voidType [stringType]
errorType = FunType Nothing voidType []
readIntType = FunType Nothing intType []
readStringType = FunType Nothing stringType []

-- This function sorts classes topologically (vertices ~ classes, edges ~ inheritances) to achieve two goals:
--  * Detect inheritance cycles - if there exists such cycle, function returns Left [n_1, n_2, ..., n_k], where
--    n_* are classes names such that n_2 extends n_1, n_3 extends n_2, ..., n_k extends n_{k - 1} and n_1 extends n_k.
--    Trivial cycles (class extends itself) are detected in buildClassHierarchy.
--  * Later on, when we want to investigate classes, we would like to analyze base class before subclass.
--    If there is no cycle in the inheritance graph, function returns Right [n_1, n_2, ..., n_k], where
--    n_* are classes names such that each n_k is either a base class or inherits from n_l for some l < k.

topologicallySortClasses :: ClassHierarchy -> Either [String] [String]
topologicallySortClasses hierarchy = go (existingClasses hierarchy) LS.empty LS.empty where
    go :: Set.Set String         -- unsorted yet vertices of the graph (classes)
       -> LS.ListSet String      -- path in graph going downward (classes [n_1, ..., n_k] such that n_1 > ... > n_k)
       -> LS.ListSet String      -- already sorted classes (in reverse order)
       -> Either [String] [String]
    go vertices path sortedVertices
        -- Start finding new path
        | LS.null path = case Set.maxView vertices of
            -- No more vertices to sort: algorithm has finished
            Nothing -> Right $ reverse $ LS.toList sortedVertices
            Just (v, vertices') -> go vertices' (LS.singleton v) sortedVertices
        -- Continue with started path
        | otherwise = case Map.lookup subclass (superclasses hierarchy) of
            -- Subclass is base class, so path is finished
            Nothing -> go vertices LS.empty sortedVertices'
            Just superclass
                -- Superclass is already sorted, so we can safely merge ListSets
                | LS.member superclass sortedVertices -> go vertices LS.empty sortedVertices'
                -- Keep going up
                | Set.member superclass vertices
                    -> go (Set.delete superclass vertices) (LS.push superclass path) sortedVertices
                -- Cycle detected
                | otherwise -> Left $ superclass : takeWhile (/= superclass) (LS.toList path)
            where subclass = LS.head path
                  sortedVertices' = LS.merge (LS.reverse path) sortedVertices

-- Entrypoint of this module, buildClassHierarchy validates hierarchy of classes
-- and returns either error message or an ordering of the classes [n_1, ..., n_k]
-- (represented by their names) such that for all indexes i class n_i either is
-- a base class or extends class n_j with j < i and structure which
-- answers queries "does class A exist?" and "is class A subclass/superclass of class B?".

buildClassHierarchy :: Program Pos -> Either String (ClassHierarchy, [String])
buildClassHierarchy (Program _ defs) = foldM analyzeDef emptyClassHierarchy defs >>= postFold where
    analyzeDef hierarchy TopFunDef{} = Right hierarchy
    analyzeDef hierarchy (BaseClassDef coords (Ident className) _) = if classExists className hierarchy
        then Left $ "Duplicate declaration of class " ++ className ++ " at " ++ show coords
        else Right $ addBaseClass className hierarchy
    analyzeDef hierarchy (ExtClassDef coords (Ident className) (Ident superclassName) _)
        | classExists className hierarchy
            = Left $ "Duplicate declaration of class " ++ className ++ " at " ++ show coords
        | className == superclassName = Left $ "Class " ++ className ++ " extends itself"
        | otherwise = Right $ addExtendedClass className superclassName hierarchy
    postFold hierarchy = case Map.foldrWithKey checkSuperclassExistence [] (superclasses hierarchy) of
        [] -> case topologicallySortClasses hierarchy of
            Left cycle -> Left $ "Cycle found in inheritance chain:\n" ++ intercalate " > " (cycle ++ [head cycle])
            Right sortedClasses -> Right (hierarchy, sortedClasses)
        t -> Left $ unlines t
        where classes = existingClasses hierarchy
              checkSuperclassExistence className superclassName errors = if Set.member superclassName classes
                then errors
                else ("Class " ++ className ++ " extends nonexistent class " ++ superclassName) : errors
