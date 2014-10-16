-- File: Real World Haskell\Ch5\Prettify.hs
module Prettify where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show,Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                Empty        -> transform ds
                Char c       -> c : transform ds
                Text s       -> s ++ transform ds
                Line         -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b  -> transform (b:ds)
                
pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char c       -> c :  best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

-- 1
fill :: Int -> Doc -> Doc
fill n = fold addSpaces . docLines
    where addSpaces lin acc
            | l < n = lin <> (text . replicate (n-l) $ ' ') <> line <> acc
            | otherwise =  lin <> line <> acc
                where l = length . pretty n $ lin

docLines :: Doc -> [Doc]
docLines = foldr buildLines [] . docList
    where docList (Concat a b) = (docList a) ++ (docList b)
          docList d = d:[]
          buildLines Line acc = empty:acc 
          buildLines (Union _ Line) acc = empty:acc
          buildLines d [] = d:[]
          buildLines d (d':ds)
            | d' == Empty = d:ds
            | otherwise = (Concat d d'):ds
          
-- 2
nest :: Int -> Doc -> Doc
nest tab doc = indent 0 (doc:[])
    where indent lv (d:ds) = case d of
            Empty        -> indent lv ds
            Text t       -> text t <> indent lv ds
            Char '{'     -> char '{' <> indent (lv+1) (line:ds)
            Char '['     -> char '[' <> indent (lv+1) (line:ds)
            Char '}'     -> indent lv (line:[]) <> char '}' <> indent (lv-1) ds
            Char ']'     -> indent lv (line:[]) <> char ']' <> indent (lv-1) ds
            Char c       -> char c <> indent lv ds
            Line         -> line <> text (replicate (tab * lv) ' ') <> indent lv ds
            a `Concat` b -> indent lv (a:b:ds)
            a `Union` b  -> indent lv (a:ds) `Union` indent lv (b:ds)
          indent _ [] = empty
