module Frontend.ClassAnalysis
    ( ClassDefinition
    , className
    , classFields
    , classMethods
    , lookupClassField
    , analyzeClasses
    , ClassesInfo
    , collectClassDefinitions
    , getClassDefinition
    ) where
import Control.Monad(foldM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Frontend.ClassHierarchy
import AbsLatte

-- The purpose of this file is to "resolve" inheritance of classes for the purpose of typechecking.
-- Each class should know all fields and methods without looking up outside of the class definition.
-- Rules of inheritance are checked:
--  * it is not possible to override fields
--  * if method is overridden, it should keep signature
--    (no covariant method return types or contravariant method parameter types)

data ClassDefinition = ClassDefinition
    { className :: String
    , classFields :: Map.Map String (Type Pos)
    , classMethods :: Map.Map String (FunType Pos)
    }

emptyClassDefinition :: String -> ClassDefinition
emptyClassDefinition className = ClassDefinition
    { className = className, classFields = Map.empty, classMethods = Map.empty }

addClassField :: String -> Type Pos -> ClassDefinition -> ClassDefinition
addClassField fieldName fieldType classDef = classDef
    { classFields = Map.insert fieldName fieldType (classFields classDef) }
    
addClassMethodType :: String -> FunType Pos -> ClassDefinition -> ClassDefinition
addClassMethodType methodName methodType classDef = classDef
    { classMethods = Map.insert methodName methodType (classMethods classDef) }

lookupClassField :: String -> ClassDefinition -> Maybe (Type Pos)
lookupClassField fieldName = Map.lookup fieldName . classFields

lookupClassMethodType :: String -> ClassDefinition -> Maybe (FunType Pos)
lookupClassMethodType methodName = Map.lookup methodName . classMethods

addField :: Type Pos
         -> ClassDefinition
         -> Ident
         -> Either String ClassDefinition
addField fieldType classDef (Ident fieldName)
    | fieldType == voidType = Left $ classId ++ " declares field of type void"
    | Just _ <- lookupClassField fieldName classDef
        = Left $ classId ++ " redeclares field " ++ fieldName
    | otherwise = Right $ addClassField fieldName fieldType classDef
    where classId = "Class " ++ className classDef
          

buildBaseClassDefinition :: String
                         -> [ClassMember Pos]
                         -> ClassHierarchy
                         -> Either String ClassDefinition
buildBaseClassDefinition className members hierarchy =
    let analyzeMember defsigns (ClassFieldDef _ fieldType idents) = foldM (addField fieldType) defsigns idents
        analyzeMember classDef (ClassMethodDef _ method)
            | Just _ <- lookupClassMethodType (name method) classDef
                = Left $ "Class " ++ className ++ " redeclares method " ++ description method
            | otherwise = case functionSignature hierarchy method of
                Left err -> Left $ "Error in signature of method " ++ description method ++ ":\n" ++ err
                Right sig -> Right $ addClassMethodType (name method) sig classDef
    in foldM analyzeMember (emptyClassDefinition className) members

mergeClassDefinitions :: ClassDefinition -> ClassDefinition -> Either String ClassDefinition
mergeClassDefinitions
    (ClassDefinition superclassName superclassFields superclassMethods)
    (ClassDefinition className classFields classMethods)
    | not (Set.null fieldsIntersection) = Left $ "Class " ++ className ++ " overrides fields "
            ++ show (Set.toList fieldsIntersection) ++ " of base class " ++ superclassName
    | not (Set.null invalidOverrides) = Left $ "Class " ++ className ++ " tries to override methods "
            ++ show (Set.toList invalidOverrides) ++ ", but signatures do not match"
    | otherwise = Right finalResult
    where fieldsIntersection = Set.intersection (Map.keysSet superclassFields) (Map.keysSet classFields)
          methodsIntersection = Set.intersection (Map.keysSet superclassMethods) (Map.keysSet classMethods)
          invalidOverrides = Set.filter
            (\methodName -> superclassMethods Map.! methodName /= classMethods Map.! methodName)
            methodsIntersection
          finalResult = ClassDefinition 
            { className = className
            , classFields = Map.union classFields superclassFields
            , classMethods = Map.union classMethods superclassMethods
            }

buildExtendedClassDefinition :: String
                             -> [ClassMember Pos]
                             -> ClassHierarchy
                             -> ClassDefinition
                             -> Either String ClassDefinition
buildExtendedClassDefinition className members hierarchy superclassDef =
    buildBaseClassDefinition className members hierarchy >>= mergeClassDefinitions superclassDef

type ClassesInfo = Map.Map String ClassDefinition

emptyClassesInfo :: ClassesInfo
emptyClassesInfo = Map.empty

getClassDefinition :: ClassesInfo -> String -> ClassDefinition
getClassDefinition = (Map.!)

updateClassesInfo :: ClassDefinition -> ClassesInfo -> ClassesInfo
updateClassesInfo classDef = Map.insert (className classDef) classDef

collectClassDefinitions :: Program Pos -> Map.Map String (TopDef Pos)
collectClassDefinitions (Program _ defs) = foldr collect Map.empty defs where
    collect :: TopDef Pos -> Map.Map String (TopDef Pos) -> Map.Map String (TopDef Pos)
    collect TopFunDef{} = id
    collect def@BaseClassDef{} = Map.insert (name def) def
    collect def@ExtClassDef{} = Map.insert (name def) def

analyzeClasses :: Program Pos -> ClassHierarchy -> [String] -> Either String ClassesInfo
analyzeClasses program hierarchy = foldM analyzeClass emptyClassesInfo where
    classTopDefs = collectClassDefinitions program
    analyzeClass classesInfo className = case classTopDefs Map.! className of
        BaseClassDef _ _ classBody -> case buildBaseClassDefinition className classBody hierarchy of
            Left err -> Left $ "Error while analyzing class " ++ className ++ ":\n" ++ err
            Right def -> Right $ updateClassesInfo def classesInfo
        ExtClassDef _ _ superclassIdent classBody ->
            let superclassDefinition = getClassDefinition classesInfo (unIdent superclassIdent)
            in case buildExtendedClassDefinition className classBody hierarchy superclassDefinition of
                Left err -> Left $ "Error while analyzing class " ++ className ++ ":\n" ++ err
                Right def -> Right $ updateClassesInfo def classesInfo
        _ -> Right classesInfo

