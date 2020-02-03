{-# LANGUAGE LambdaCase #-}
module Frontend.ConstantPropagation(propagateConstants) where
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import AbsLatte
import Frontend.Environment

-- Rules of propagating constants:
--  * fields of objects are not investigated at all,
--  * values returned by functions/methods are not investigated at all,
--  * arrays are not investigated at all.
-- Locations could point to empty memory.
-- That happens when we erase constants (because of conditional branches).

data ConstEnv = ConstEnv
    { variables :: Map.Map String Int
    , errTrace :: [String]
    , modifiedLocations :: Set.Set Int
    }

instance Environment ConstEnv where
    updateTrace tr env = env { errTrace = tr : errTrace env }
    getTrace = errTrace
    dropTrace env = env { errTrace = tail (errTrace env) }

updateEnv :: String -> Int -> ConstEnv -> ConstEnv
updateEnv var loc env = env { variables = Map.insert var loc (variables env) }
lookupEnv :: String -> ConstEnv -> Maybe Int
lookupEnv var = Map.lookup var . variables
addModifiedLocation :: Int -> ConstEnv -> ConstEnv
addModifiedLocation loc env = env { modifiedLocations = Set.insert loc (modifiedLocations env) }
initConstEnv :: ConstEnv
initConstEnv = ConstEnv
    { variables = Map.empty
    , errTrace = []
    , modifiedLocations = Set.empty
    }

bToE :: Bool -> Constant Pos
bToE True = CTrue Nothing
bToE False = CFalse Nothing

initStore :: Store (Constant Pos)
initStore = mkStore Map.empty

type ConstM = SemM (Constant Pos) ConstEnv
type Prop a = a Pos -> ConstM (a Pos)

lookupM :: String -> ConstM (Maybe (Constant Pos))
lookupM var = runMaybeT (MaybeT (asks $ lookupEnv var) >>= MaybeT . gets . lookupStore)

-- We know that code is properly typed; this allows to write code more carelessly.

voidDeclErr :: ConstM a
voidDeclErr = throwTrace "FATAL ERROR: declaration of variables of type void slipped through typechecker"

-- Branching instructions may modify store and change knowledge about constants.
-- When propagating, we want to find all variables that could be modified
-- and remove information about them from store.

collectChangesLVal :: LVal Pos -> ConstM ConstEnv
collectChangesLVal = \case
    LValVar _ ident -> asks (lookupEnv $ unIdent ident) >>= maybe ask ((<$> ask) . addModifiedLocation)
    _ -> ask

collectChangesStmt :: Stmt Pos -> ConstM ConstEnv
collectChangesStmt = \case
    SEmpty{} -> ask
    SBlock _ block -> collectChangesStmtBlock block
    SDecl _ decl -> collectChangesDecl decl
    SAssign _ lval _ -> collectChangesLVal lval
    SIncr _ lval -> collectChangesLVal lval
    SDecr _ lval -> collectChangesLVal lval
    SRet{} -> ask
    SRetVoid{} -> ask
    SIf _ _ stmt -> collectBranch stmt
    SIfte _ _ trueStmt falseStmt -> do
        env <- ask
        trueLocs <- modifiedLocations <$> collectChangesStmt trueStmt
        falseLocs <- modifiedLocations <$> collectChangesStmt falseStmt
        return env { modifiedLocations = trueLocs <> falseLocs }
    SWhile _ _ stmt -> collectBranch stmt
    SFor _ _ _ _ stmt -> collectBranch stmt -- variable of this loop is not in the new block
    SExp{} -> ask
    where collectBranch stmt = do
            env <- ask
            locs <- modifiedLocations <$> collectChangesStmt stmt
            return env { modifiedLocations = locs }

collectChangesDecl :: Decl Pos -> ConstM ConstEnv
collectChangesDecl = (ask >>=) . flip (foldM collectChangesDeclItem) . declItems

collectChangesDeclItem :: ConstEnv -> DeclItem Pos -> ConstM ConstEnv
collectChangesDeclItem env item = do
    loc <- allocM
    return (updateEnv (name item) loc env)

collectChangesStmtBlock :: Block Pos -> ConstM ConstEnv
collectChangesStmtBlock (StmtBlock _ stmts) = do
    env <- ask
    locs <- modifiedLocations <$> foldM collectChangesStmt' (updateTrace "in block of statements" env) stmts
    return env { modifiedLocations = locs }
    where collectChangesStmt' baseEnv stmt = local (const baseEnv) (collectChangesStmt stmt)

desugarFor :: Stmt Pos -> Stmt Pos
desugarFor (SFor pos t ident exp stmt) = SBlock pos $ StmtBlock pos
    [ SDecl pos (VarDecl pos (ArrayType pos t) [DeclWithInit pos identA exp])
    , SDecl pos $ VarDecl pos (IntType pos)
        [ DeclWithInit pos identL $ ELVal pos $ LValAttr pos (LValVar pos identA) (Ident "length")
        , DeclWithInit pos identI $ EConstant pos $ CInteger pos 0
        ]
    , SDecl pos (VarDecl pos t [DeclNoInit pos ident])
    , SWhile pos (EComp pos expI opLt expL) $ SBlock pos $ StmtBlock pos
        [ SAssign pos (LValVar pos ident) (ELVal pos $ LValArrAccess pos (LValVar pos identA) expI)
        , stmt
        , SIncr pos $ LValVar pos identI
        ]
    ] where mkIdent str = Ident (unIdent ident ++ str)
            identA = mkIdent "..a"
            identL = mkIdent "..l"
            identI = mkIdent "..i"
            expI = ELVal pos $ LValVar pos identI
            expL = ELVal pos $ LValVar pos identL

propagateTopDef :: TopDef Pos -> ConstM (TopDef Pos)
propagateTopDef = \case
    TopFunDef pos funDef -> TopFunDef pos <$> propagateFunDef funDef
    BaseClassDef pos ident members
        -> BaseClassDef pos ident <$> mapM propagateClassMember members
    ExtClassDef pos ident ident2 members
        -> ExtClassDef pos ident ident2 <$> mapM propagateClassMember members

propagateClassMember :: Prop ClassMember
propagateClassMember = \case
    ClassMethodDef pos methodDef -> ClassMethodDef pos <$> propagateFunDef methodDef
    fieldDef -> return fieldDef

propagateFunDef :: Prop FunDef
propagateFunDef def = do
    -- We don't add arguments of function, since their value is unknown
    block' <- local (updateTrace ("in body of function " ++ name def)) (propagateStmtBlock $ funBlock def)
    let retV = SRetVoid Nothing
    let endsWithRet = \case { [] -> False; l -> last l == retV }
    let block'' | funRetType def /= voidType = block'
                | endsWithRet stmts = block'
                | otherwise = block' { blockStmts = stmts ++ [retV] }
                where stmts = blockStmts block'
    return def { funBlock = block'' }
    where addArg :: ConstEnv -> String -> ConstM ConstEnv
          addArg env var = allocM >>= \loc -> return (updateEnv var loc env)

propagateStmtBlock :: Prop Block
propagateStmtBlock (StmtBlock pos stmts) = StmtBlock pos . snd <$> propagateStmts stmts
    where propagateStmts :: [Stmt Pos] -> ConstM (ConstEnv, [Stmt Pos])
          propagateStmts [] = ask >>= \env -> return (env, [])
          propagateStmts (stmt:stmts) = do
            (env', stmt') <- propagateStmt stmt
            (env'', stmts') <- local (const env') (propagateStmts stmts)
            return (env'', stmt':stmts')

propagateStmt :: Stmt Pos -> ConstM (ConstEnv, Stmt Pos)
propagateStmt stmt = ask >>= \env -> let ret = return (env, stmt) in case stmt of
    SEmpty{} -> ret
    SBlock pos block -> propagateStmtBlock block >>= \block' -> return (env, SBlock pos block')
    SDecl pos decl -> propagateDecl decl >>= \(env', decl') -> return (env', SDecl pos decl')
    SAssign pos lval exp -> do
        exp' <- propagateExp exp
        case (exp', lval) of
            (EConstant _ c, LValVar _ ident) -> asks (lookupEnv $ unIdent ident) >>= maybe
                (return ()) (\loc -> modify (updateStore loc c))
            (_, LValVar _ ident) -> asks (lookupEnv $ unIdent ident) >>= maybe
                (return ()) (modify . removeStore)
            _ -> return ()
        return (env, SAssign pos lval exp')
    SIncr _ lval -> modifyVar (name lval) succ >> ret
    SDecr _ lval -> modifyVar (name lval) pred >> ret
    SRet pos exp -> propagateExp exp >>= \exp' -> return (env, SRet pos exp')
    SRetVoid pos -> return (env, SRetVoid pos)
    SExp pos exp -> propagateExp exp >>= \exp' -> return (env, SExp pos exp')
    -- In case of branching instructions the plan is to:
    --  * compute set of variables that could be modified in branch
    --  * drop their values from store, let S denote such a store
    --  * propagate loop condition under:
    --    - old assumptions in case or if/ifte/for
    --    - assumptions S in case of while
    --  * propagate statements in body of the loop under assumptions S
    --  * continue traversing code under assumptions S
    SIf pos exp ifStmt -> do
        exp' <- propagateExp exp
        store <- buildDiffStore ifStmt
        ifStmt' <- snd <$> propagateStmt ifStmt
        put store
        return (env, SIf pos exp' ifStmt')
    SIfte pos exp trueStmt falseStmt -> do
        trueLocs <- modifiedLocations <$> collectChangesStmt trueStmt
        falseLocs <- modifiedLocations <$> collectChangesStmt falseStmt
        (preStore, ctr) <- get
        let store = (Map.withoutKeys preStore (trueLocs <> falseLocs), ctr)
        put store
        exp' <- propagateExp exp
        trueStmt' <- snd <$> propagateStmt trueStmt
        put store
        falseStmt' <- snd <$> propagateStmt falseStmt
        put store
        return (env, SIfte pos exp' trueStmt' falseStmt')
    SWhile pos exp loopStmt -> do
        store <- buildDiffStore loopStmt
        exp' <- propagateExp exp
        loopStmt' <- snd <$> propagateStmt loopStmt
        put store
        return (env, SWhile pos exp' loopStmt')
    SFor{} -> propagateStmt (desugarFor stmt)
    where modifyVar :: String -> (Integer -> Integer) -> ConstM (Maybe ())
          modifyVar var f = runMaybeT $ do
            loc <- MaybeT $ asks (lookupEnv var)
            c <- MaybeT $ gets (lookupStore loc)
            case c of CInteger pos n -> modify $ updateStore loc $ CInteger pos $ f n
                      _ -> return ()
          buildDiffStore stmt = do
            locs <- modifiedLocations <$> collectChangesStmt stmt
            (preStore, ctr) <- get
            let store = (Map.withoutKeys preStore locs, ctr)
            put store
            return store
            

propagateDecl :: Decl Pos -> ConstM (ConstEnv, Decl Pos)
propagateDecl (VarDecl pos t items) = if t == voidType
    then voidDeclErr
    else do env <- ask
            (env', items') <- foldlMapM (propagateDeclItem t) env items
            return (env', VarDecl pos t items')
    where foldlMapM _ x [] = return (x, [])
          foldlMapM f x (y:ys) = do
            (x', y') <- f x y
            (x'', ys') <- foldlMapM f x' ys
            return (x'', y':ys')

defaultValue :: Type Pos -> ConstM (Exp Pos)
defaultValue t = case t of
    IntType _ -> return zero
    StringType _ -> return $ EConstant Nothing $ CString Nothing ""
    BoolType _ -> return $ EConstant Nothing $ CFalse Nothing
    VoidType _ -> voidDeclErr
    ArrayType _ elemT -> return $ ENewArr Nothing elemT zero
    _ -> return $ ENullObj Nothing $ Ident (name t)
    where zero = EConstant Nothing $ CInteger Nothing 0

propagateDeclItem :: Type Pos -> ConstEnv -> DeclItem Pos -> ConstM (ConstEnv, DeclItem Pos)
-- This replaces declarations without initializations
propagateDeclItem t env decl@(DeclNoInit pos ident) = do
    val <- defaultValue t
    env <- fst <$> propagateDeclItem t env (DeclWithInit pos ident val)
    return (env, decl)
propagateDeclItem _ env (DeclWithInit pos ident exp) = do
    exp' <- local (const env) (propagateExp exp)
    loc <- case exp' of
        EConstant _ c -> allocInsertM c
        _ -> allocM
    return (updateEnv (unIdent ident) loc env, DeclWithInit pos ident exp')

propagateCall :: Prop Call
propagateCall (FunCall pos ident exps) = FunCall pos ident <$> mapM propagateExp exps

-- If lvalue is a variable name, we could replace it with constant or not replace at all.
-- If lvalue is a call, we could propagate constants in arguments.
propagateLVal :: LVal Pos -> ConstM (Either (Exp Pos) (LVal Pos))
propagateLVal = \case
    lval@(LValVar pos ident) -> maybe (Right lval) (Left . EConstant pos) <$> lookupM (unIdent ident)
    LValCall pos call -> Right . LValCall pos <$> propagateCall call
    LValMethod pos lval call -> Right <$> (LValMethod pos <$> (propagateLVal lval >>= fromRight) <*> propagateCall call)
    LValArrAccess pos lval exp -> Right <$> (LValArrAccess pos <$> (propagateLVal lval >>= fromRight) <*> propagateExp exp)
    lval -> return (Right lval)
    where fromRight :: Either (Exp Pos) (LVal Pos) -> ConstM (LVal Pos)
          fromRight (Right x) = return x
          fromRight (Left _) = throwTrace "FATAL ERROR: lval should not be propagated to exp"

propagateExp :: Prop Exp
propagateExp exp = case exp of
    ELVal pos lval -> propagateLVal lval >>= \case
        Left exp' -> return exp'
        Right lval' -> return $ ELVal pos lval'
    ENewArr pos t exp -> ENewArr pos t <$> propagateExp exp
    ENegate pos op exp1 -> propagateExp exp1 >>= \case
        EConstant _ (CInteger _ n) -> return $ EConstant pos $ CInteger pos $ negate n
        EConstant _ (CTrue _) -> return $ EConstant pos $ CFalse pos
        EConstant _ (CFalse _) -> return $ EConstant pos $ CTrue pos
        exp1' -> return $ ENegate pos op exp1'
    EMod pos exp1 op exp2 -> do
        exp1' <- propagateExp exp1
        exp2' <- propagateExp exp2
        case (exp1', exp2') of
            (EConstant _ (CInteger _ n), EConstant _ (CInteger _ m)) -> if op /= opTimes && m == 0
                then throwError "Division by zero detected while propagating constant"
                else return $ EConstant pos $ CInteger pos $ op' n m
            _ -> return (EMod pos exp1' op exp2')
            where op' = case op of OpTimes _ -> (*)
                                   OpDiv _ -> div
                                   OpMod _ -> mod
    EAdd pos exp1 op exp2 -> do
        exp1' <- propagateExp exp1
        exp2' <- propagateExp exp2
        case (exp1', exp2') of
            (EConstant _ (CInteger _ n), EConstant _ (CInteger _ m))
                -> return $ EConstant pos $ CInteger pos $ op' n m
            (EConstant _ (CString _ s), EConstant _ (CString _ t))
                -> return $ EConstant pos $ CString pos $ s ++ t
            _ -> return $ EAdd pos exp1' op exp2'
            where op' = case op of OpPlus _ -> (+)
                                   OpMinus _ -> (-)
    EComp pos exp1 op exp2 -> do
        exp1' <- propagateExp exp1
        exp2' <- propagateExp exp2
        case (exp1', exp2') of
            (EConstant _ (CInteger _ n), EConstant _ (CInteger _ m))
                -> return $ EConstant pos $ bToE $ op' n m
            (EConstant _ (CString _ s), EConstant _ (CString _ t))
                -> return $ EConstant pos $ bToE $ op' s t
            (EConstant _ (CTrue _), EConstant _ (CFalse _))
                -> return $ EConstant pos $ bToE $ op' True False
            (EConstant _ (CFalse _), EConstant _ (CFalse _))
                -> return $ EConstant pos $ bToE $ op' False False
            (EConstant _ (CTrue _), EConstant _ (CTrue _))
                -> return $ EConstant pos $ bToE $ op' True True
            (EConstant _ (CFalse _), EConstant _ (CTrue _))
                -> return $ EConstant pos $ bToE $ op' False True
            _ -> return $ EComp pos exp1' op exp2'
            where op' :: Ord a => a -> a -> Bool
                  op' = case op of OpLt _ -> (<)
                                   OpLeq _ -> (<=)
                                   OpGt _ -> (>)
                                   OpGeq _ -> (>=)
                                   OpEqual _ -> (==)
                                   OpNeq _ -> (/=)
    EAnd pos exp1 exp2 -> do
        exp1' <- propagateExp exp1
        exp2' <- propagateExp exp2
        case exp1' of
            EConstant _ (CFalse _) -> return exp1'
            EConstant _ (CTrue _) -> return exp2'
            _ -> return $ EAnd pos exp1' exp2'
    EOr pos exp1 exp2 -> do
        exp1' <- propagateExp exp1
        exp2' <- propagateExp exp2
        case exp1' of
            EConstant _ (CFalse _) -> return exp2'
            EConstant _ (CTrue _) -> return exp1'
            _ -> return $ EOr pos exp1' exp2'
    _ -> return exp

propagateProgram :: Prop Program
propagateProgram (Program pos defs) = Program pos <$> mapM propagateTopDef defs

propagateConstants :: Program Pos -> Either String (Program Pos)
propagateConstants program = runSemM (propagateProgram program) initStore initConstEnv