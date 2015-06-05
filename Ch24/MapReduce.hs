-- File: Real World Haskell/Ch24/MapReduce.hs
module MapReduce where

import Control.Parallel.Strategies
import Control.Parallel

simpleMapReduce
    :: (a -> b)      -- map function
    -> ([b] -> c)    -- reduce function
    -> [a]           -- list to map over
    -> c
simpleMapReduce mapFunc reduceFunc = reduceFunc . map mapFunc


mapReduce
    :: Strategy b    -- evaluation strategy for mapping
    -> (a -> b)      -- map function
    -> Strategy c    -- evaluation strategy for reduction
    -> ([b] -> c)    -- reduce function
    -> [a]           -- list to map over
    -> c
        -- In the formal definition of MapReduce the fold function has to be of type [b] -> b,
        -- and the property reduce x:() = x must hold, s.t. it can be parallelised.
        -- e.g. r1 = reduce(xs1); r2 = reduce(xs2); ...; rn = reduce(xsn); rfinal = reduce([r1,r2,...,rn])
mapReduce mapStrat mapFunc reduceStrat reduceFunc input =
    mapResult `pseq` reduceResult
  where mapResult    = parMap mapStrat mapFunc input
        reduceResult = reduceFunc mapResult `using` reduceStrat
