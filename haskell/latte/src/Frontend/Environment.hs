module Frontend.Environment
    ( Environment
    , Store
    , mkStore
    , alloc
    , updateTrace
    , dropTrace
    , getTrace
    , updateStore
    , lookupStore
    , removeStore
    , SemM
    , runSemM
    , allocM
    , allocInsertM
    , throwTrace
    ) where
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Reader
import qualified Data.Map.Strict as Map

-- Responsibility for building store and environment is shifted to the file using this module.
-- Each one will be a bit different, but all environments should produce informative
-- error messages in case of failure.
type Store a = (Map.Map Int a, Int)
mkStore :: Map.Map Int a -> Store a
mkStore store = (store, maybe 0 (succ . fst) (Map.lookupMax store))

class Environment a where
    updateTrace :: String -> a -> a
    dropTrace :: a -> a
    getTrace :: a -> [String]

alloc :: Store a -> (Int, Store a)
alloc (store, ctr) = (ctr, (store, ctr + 1))
updateStore :: Int -> a -> Store a -> Store a
updateStore loc val (store, ctr) = (Map.insert loc val store, ctr)
lookupStore :: Int -> Store a -> Maybe a
lookupStore loc = Map.lookup loc . fst
removeStore :: Int -> Store a -> Store a
removeStore loc (store, ctr) = (Map.delete loc store, ctr)

type SemM store env = StateT (Store store) (ReaderT env (Except String))

runSemM :: SemM store env a -> Store store -> env -> Either String a
runSemM op store env = runExcept (runReaderT (evalStateT op store) env)

allocM :: SemM store env Int
allocM = do
    (loc, store) <- gets alloc
    put store
    return loc

allocInsertM :: store -> SemM store env Int
allocInsertM mem = do
    loc <- allocM
    modify (updateStore loc mem)
    return loc

throwTrace :: Environment env => String -> SemM store env a
throwTrace err = asks getTrace >>= throwError . unlines . (err:)