-- File: Real World Haskell\Ch11\QC.hs
import Arbitrary

prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x