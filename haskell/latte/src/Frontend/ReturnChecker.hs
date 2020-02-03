{-# LANGUAGE LambdaCase #-}
module Frontend.ReturnChecker(checkReturns) where
import Control.Monad(mapM_)
import AbsLatte

blockReturns :: Block Pos -> Bool
blockReturns = any returns . blockStmts where
    returns = \case
        SBlock _ block -> blockReturns block
        SRet{} -> True
        SRetVoid{} -> True
        SIf _ exp whileStmt -> case exp of
            EConstant _ c -> c == cTrue && returns whileStmt
            _ -> False
        SIfte _ _ trueStmt falseStmt -> returns trueStmt && returns falseStmt
        SWhile _ exp whileStmt -> case exp of
            EConstant _ c -> c == cTrue && returns whileStmt
            _ -> False
        _ -> False

checkTopDef :: TopDef Pos -> Either String ()
checkTopDef = \case
    TopFunDef _ (FunDef _ t ident _ block) -> if t == voidType || blockReturns block
        then Right ()
        else Left $ "Return checker could not prove that function " ++ unIdent ident ++ " always returns"
    BaseClassDef _ ident members -> mapM_ (checkClassMember $ unIdent ident) members
    ExtClassDef _ ident _ members -> mapM_ (checkClassMember $ unIdent ident) members

checkClassMember :: String -> ClassMember Pos -> Either String ()
checkClassMember className = \case
    ClassFieldDef{} -> Right ()
    ClassMethodDef _ (FunDef _ t ident _ block) -> if t == voidType || blockReturns block
        then Right ()
        else Left $ "Return checker could not prove that method " ++ unIdent ident ++ " of class "
                 ++ className ++ " always returns"

checkReturns :: Program Pos -> Either String ()
checkReturns (Program _ defs) = mapM_ checkTopDef defs