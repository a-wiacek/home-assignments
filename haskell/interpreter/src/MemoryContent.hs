module MemoryContent where

import qualified Data.Map.Strict as Map
import Control.Monad.Except
import AbsGrammar

data StaticVar = StaticVar { declPos :: (Int, Int), loc :: Loc }

data FuncBody
    = FuncBody StmtBlock
    | GetInput | Print | ToString
    | StringToInteger | StringToDouble

data FunctionInMemory = FuncInMem
    { funcArgs :: [FuncArg]
    , funcArgVars :: [Var]
    , returnType :: ExpType
    , staticVars :: [StaticVar]
    , funcBody :: FuncBody 
    , closure :: Env }
    -- Closure does not contain information about arguments of function
    -- or static variables, but it does contain function itself.

data MemoryType
    = MemVarType VarType
    | MemFuncType FunctionInMemory

data MemoryContent
    = String String
    | Integer Integer
    | Bool Bool
    | Double Double
    | Null
    | MemFunc FunctionInMemory

type Var = String
type Loc = Integer
nullloc :: Loc
nullloc = -1
type Env = Map.Map Var Loc
type Store a = Map.Map Loc a
type TypeStore = Store MemoryType
type ExecStore = Store MemoryContent

alloc :: Store a -> Loc
alloc s = maybe 0 ((+1) . fst) $ Map.lookupMax s

updateEnv :: Var -> Loc -> Env -> Env
updateEnv = Map.insert
lookupEnv :: Var -> Env -> Maybe Loc
lookupEnv = Map.lookup
updateStore :: Loc -> a -> Store a -> Store a
updateStore = Map.insert
lookupStore :: Loc -> Store a -> Maybe a
lookupStore = Map.lookup

initEnv = Map.fromList
    [ ("getInput", 0)
    , ("printInteger", 1)
    , ("printString", 2)
    , ("printDouble", 3)
    , ("stringToInteger", 4)
    , ("stringToDouble", 5)
    , ("integerToString", 6)
    , ("doubleToString", 7) ]

initStore cons = Map.fromList $ zip [0..] $ map cons
    [ FuncInMem [] [] (ExpType StringType) [] GetInput initEnv
    , FuncInMem [FuncArgVal IntegerType] [] VoidType [] Print initEnv
    , FuncInMem [FuncArgVal StringType] [] VoidType [] Print initEnv
    , FuncInMem [FuncArgVal DoubleType] [] VoidType [] Print initEnv
    , FuncInMem [FuncArgVal StringType] [] (ExpType IntegerType) [] StringToInteger initEnv
    , FuncInMem [FuncArgVal StringType] [] (ExpType DoubleType) [] StringToDouble initEnv
    , FuncInMem [FuncArgVal IntegerType] [] (ExpType StringType) [] ToString initEnv
    , FuncInMem [FuncArgVal DoubleType] [] (ExpType StringType) [] ToString initEnv ]

initTypeStore = initStore MemFuncType
initExecStore = initStore MemFunc

