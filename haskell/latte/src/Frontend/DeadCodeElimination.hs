{-# LANGUAGE LambdaCase #-}
module Frontend.DeadCodeElimination(eliminateDeadCode) where

import AbsLatte

-- Remove unreachable code (if (false) {..}, code after while (true) {..} and after return)

-- Bool result tells whether it is possible to reach the end of the block/list of statements
investigateStmtListReachability :: [Stmt Pos] -> ([Stmt Pos], Bool)
investigateStmtListReachability [] = ([], True)
investigateStmtListReachability (stmt:stmts) = case stmt of
    SBlock pos block -> let (block', b) = investigateBlockReachability block
                        in (if b then continue else terminate) (SBlock pos block')
    SRet{} -> terminate stmt
    SRetVoid{} -> terminate stmt
    SIf pos exp ifStmt -> case exp of
        EConstant _ (CFalse _) -> end
        EConstant _ (CTrue _) -> investigateStmtListReachability (ifStmt:stmts)
        _ -> case fst $ investigateStmtListReachability [ifStmt] of
            [] -> end
            [s] -> continue $ SIf pos exp s
    SIfte pos exp trueStmt falseStmt -> case exp of
        EConstant _ (CTrue _) -> investigateStmtListReachability (trueStmt:stmts)
        EConstant _ (CFalse _) -> investigateStmtListReachability (falseStmt:stmts)
        _ -> case map (fst . investigateStmtListReachability) [[trueStmt], [falseStmt]] of
            [[], []] -> end
            [[ts], []] -> continue $ SIf pos exp ts
            [[], [fs]] -> continue $ SIf pos (ENegate pos opNot exp) fs
            [[ts], [fs]] -> continue $ SIfte pos exp ts fs
    SWhile pos exp loopStmt -> case exp of
        EConstant _ (CFalse _) -> end
        EConstant _ (CTrue _) -> terminate while
        _ -> continue while
        where while = SWhile pos exp $ case fst $ investigateStmtListReachability [loopStmt] of 
                [] -> SEmpty pos
                [s] -> s
    -- SFor: we ignore that because keeping track of the size of the array is too much
    _ -> continue stmt
    where end@(stmts', endReachable) = investigateStmtListReachability stmts
          continue s = (s:stmts', endReachable)
          terminate s = ([s], False)

investigateBlockReachability :: Block Pos -> (Block Pos, Bool)
investigateBlockReachability (StmtBlock pos stmts) = 
    let (stmts', b) = investigateStmtListReachability stmts
    in (StmtBlock pos stmts', b)

removeUnreachableCodeBlock :: Block Pos -> Block Pos
removeUnreachableCodeBlock = fst . investigateBlockReachability

removeUnreachableCodeFunDef :: FunDef Pos -> FunDef Pos
removeUnreachableCodeFunDef def = def { funBlock = removeUnreachableCodeBlock (funBlock def) }

removeUnreachableCodeClassMember :: ClassMember Pos -> ClassMember Pos
removeUnreachableCodeClassMember = \case
    ClassMethodDef pos funDef -> ClassMethodDef pos $ removeUnreachableCodeFunDef funDef
    m -> m

removeUnreachableCodeTopDef :: TopDef Pos -> TopDef Pos
removeUnreachableCodeTopDef = \case
    TopFunDef pos funDef -> TopFunDef pos $ removeUnreachableCodeFunDef funDef
    BaseClassDef pos ident members
        -> BaseClassDef pos ident $ map removeUnreachableCodeClassMember members
    ExtClassDef pos ident1 ident2 members
        -> ExtClassDef pos ident1 ident2 $ map removeUnreachableCodeClassMember members

removeUnreachableCode :: Program Pos -> Program Pos
removeUnreachableCode (Program pos defs) = Program pos $ map removeUnreachableCodeTopDef defs

eliminateDeadCode :: Program Pos -> Either String (Program Pos)
eliminateDeadCode = return . removeUnreachableCode