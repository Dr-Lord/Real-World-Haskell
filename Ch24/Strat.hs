-- File: Real World Haskell/Ch24/Strat.hs

-- NOTE: In Control.Parallel.Strategies seq is redefined as an alias of pseq.
--       Remember this in following functions

type Done = ()

type Strategy a = a -> Done -- Older definition
type Strategy a = a -> Eval a -- Current Definition

    -- Evaluation strategy which does nothing
r0 :: Strategy a
r0 _ = ()

    -- Evaluation strategy which evaluates a value to Weak Head Normal Form
rwhnf :: Strategy a
rwhnf x = x `seq` ()


class NFData a where -- Older definition
  rnf :: Strategy a
  rnf = rwhnf
class NFData a where rnf :: a -> () -- Current definition; currently in Control.DeepSeq; notice rnf is no longer a Strategy


instance NFData Char
instance NFData Int

instance NFData a => NFData (Maybe a) where
    rnf Nothing  = ()
    rnf (Just x) = rnf x


parList :: Strategy a -> Strategy [a]
parList strat []     = ()
parList strat (x:xs) = strat x `par` (parList strat xs)

parMap :: Strategy b -> (a -> b) -> [a] -> [b]
parMap strat f xs = map f xs `using` parList strat

using :: a -> Strategy a -> a
using x s = s x `seq` x


vectorSum' :: (NFData a, Num a) => [a] -> [a] -> [a]
vectorSum' = parZipWith rnf (+)
