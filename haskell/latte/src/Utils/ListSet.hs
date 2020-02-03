module Utils.ListSet
    ( ListSet
    , empty
    , singleton
    , Utils.ListSet.null
    , Utils.ListSet.head
    , toList
    , fromList
    , push
    , pop
    , member
    , Utils.ListSet.reverse
    , merge
    ) where
import qualified Data.Set as Set

-- Combination of list and set. Keeps elements in certain order (list) but also
-- allows to check membership in O(log n) (set) at the cost of O(log n) push/pop.
-- No duplicate elements allowed.

data ListSet a = ListSet [a] (Set.Set a) deriving Show
instance Eq a => Eq (ListSet a) where ListSet l1 _ == ListSet l2 _ = l1 == l2

empty :: ListSet a
empty = ListSet [] Set.empty

singleton :: a -> ListSet a
singleton x = ListSet [x] $ Set.singleton x

null :: ListSet a -> Bool
null (ListSet [] _) = True
null _ = False

-- This is just as unsafe as head for lists!
head :: ListSet a -> a
head (ListSet [] _) = error "ListSet.head: empty"
head (ListSet (h:_) _) = h

toList :: ListSet a -> [a]
toList (ListSet l _) = l

fromList :: Ord a => [a] -> ListSet a
fromList l = ListSet l (Set.fromList l)

push :: Ord a => a -> ListSet a -> ListSet a
push x (ListSet l s) = if Set.member x s
    then ListSet l s
    else ListSet (x:l) (Set.insert x s)

pop :: Ord a => ListSet a -> Maybe (a, ListSet a)
pop (ListSet [] _) = Nothing
pop (ListSet (h:t) s) = Just (h, ListSet t $ Set.delete h s)

member :: Ord a => a -> ListSet a -> Bool
member x (ListSet _ s) = Set.member x s

reverse :: ListSet a -> ListSet a
reverse (ListSet l s) = ListSet (Prelude.reverse l) s

-- It is assumed that sets s1 and s2 are disjoint.
merge :: Ord a => ListSet a -> ListSet a -> ListSet a
merge (ListSet l1 s1) (ListSet l2 s2) = ListSet (l1 ++ l2) (s1 <> s2)