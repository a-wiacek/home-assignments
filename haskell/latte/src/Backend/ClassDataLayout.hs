module Backend.ClassDataLayout
    ( ClassDataLayout(..)
    , ClassesDataLayout
    , mkLlvmClassesData
    , buildClassesDataLayout
    ) where
import Data.List(foldl', sort)
import qualified Data.Map.Strict as Map
import Data.Tuple(swap)
import Text.Printf

import AbsLatte
import Frontend.ClassHierarchy(ClassHierarchy, getLlvmFunType)
import Frontend.ClassAnalysis(collectClassDefinitions)
import Backend.LLVMData

-- The layout of fields in LLVM structure representing class is as follows:
--  * 0th position points to virtual table of the class
--  * next positions point to class fields
-- Names of produced methods are "@method.B.f" for method f of class B.
-- Class constructor is stored outside of the virtual table.
-- Names of constructors are "@constructor.B" for class B.
-- Virtual tables have type [n * void (...)*] when in global namespace,
-- but void (...)** when in class field.
-- Tables have name "@vtable.B" for class B.

data ClassDataLayout = ClassDataLayout
    { virtualTable :: Map.Map String (Int, LlvmType, String)
    , fieldsLayout :: Map.Map String (Int, LlvmType)
    }

type ClassesDataLayout = Map.Map String ClassDataLayout

mkLlvmClassData :: String -> ClassDataLayout -> (LlvmVTable, LlvmGlobalTypeDecl)
mkLlvmClassData className (ClassDataLayout vtable layout) =
    ( LlvmVTable className $ map mkVTableEntry $ sort $ map swap $ Map.toList vtable
    , LlvmGlobalTypeDecl className $ map (snd . fst) $ sort $ map swap $ Map.toList layout )
    where mkVTableEntry ((_, t, className), methodName)
            = (t , LlvmIdent (printf "method.%s.%s" className methodName) True)

mkLlvmClassesData :: ClassesDataLayout -> Map.Map String (LlvmVTable, LlvmGlobalTypeDecl)
mkLlvmClassesData = Map.mapWithKey mkLlvmClassData

addMemberDef :: String -> ClassMember Pos -> ClassDataLayout -> ClassDataLayout
addMemberDef className (ClassFieldDef _ t idents) (ClassDataLayout vtable fields)
    = ClassDataLayout vtable $ foldr (addFieldDef $ typeToLlvmType t) fields idents
addMemberDef className (ClassMethodDef _ funDef) (ClassDataLayout vtable fields) =
    let index = case Map.lookup (name funDef) vtable of
            Nothing -> Map.size vtable
            Just (i, _, _) -> i -- replace method from base class
    in ClassDataLayout (Map.insert (name funDef) (index, getLlvmFunType className funDef, className) vtable) fields

addFieldDef :: LlvmType -> Ident -> Map.Map String (Int, LlvmType) -> Map.Map String (Int, LlvmType)
addFieldDef t (Ident s) m = Map.insert s (Map.size m + 1, t) m

formBaseClassLayout :: String -> [ClassMember Pos] -> ClassDataLayout
formBaseClassLayout className = foldr (addMemberDef className) (ClassDataLayout Map.empty Map.empty)

formExtendedClassLayout :: String -> ClassDataLayout -> [ClassMember Pos] -> ClassDataLayout
formExtendedClassLayout = foldr . addMemberDef

buildClassesDataLayout :: Program Pos -> ClassHierarchy -> [String] -> ClassesDataLayout
buildClassesDataLayout program hierarchy = foldl' formClassLayout Map.empty where
    classTopDefs = collectClassDefinitions program
    formClassLayout classesLayout className = case classTopDefs Map.! className of
        BaseClassDef _ _ classBody -> Map.insert className (formBaseClassLayout className classBody) classesLayout
        ExtClassDef _ _ (Ident superclassName) classBody
            -> Map.insert className
                          (formExtendedClassLayout className (classesLayout Map.! superclassName) classBody)
                          classesLayout
