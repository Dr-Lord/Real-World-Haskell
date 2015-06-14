-- File: Real World Haskell\Ch11\QC.hs
import Arbitrary
import Prettify2
import Test.QuickCheck

prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x


prop_char c   = char c   == Char c

prop_text s   = text s   == if null s then Empty else Text s

prop_line     = line     == Line

prop_double d = double d == text (show d)
