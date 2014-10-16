-- File: Real World Haskell\Ch10\TreeMap.hs

data Tree a = Node (Tree a) (Tree a)
            | Leaf a
              deriving (Show)


treeLengths (Leaf s)   = Leaf (length s)
treeLengths (Node l r) = Node (treeLengths l) (treeLengths r)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a)   = Leaf (f a)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)

-- class Functor f where
    -- fmap :: (a -> b) -> f a -> f b

    -- Functor Laws
-- fmap id       ==  id
-- fmap (f . g)  ==  fmap f . fmap g

instance Functor Tree where
    fmap = treeMap
