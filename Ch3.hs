-- File: Real World Haskell\Ch3.hs
import Data.List

-- 1,2
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 3
listMean :: [Double] -> Double
listMean list = (sum list) / fromIntegral (length list)

-- 4
palindromize :: [a] -> [a]
palindromize list = list ++ rev list
    where rev [] = []
          rev xs = [last xs] ++ rev (init xs)
 -- OR
palindromize' :: [a] -> [a]
palindromize' [] = []
palindromize' (x:xs) = [x]++(palindromize' xs)++[x]

-- 5
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs) = (x == last xs) && isPalindrome (init xs)

-- 6
lenSort :: [[a]] -> [[a]]
lenSort list = sortBy criteria list
    where criteria xs ys = compare (length xs) (length ys)

-- 7
inters' :: a -> [a] -> [a]
inters' sep (x:[]) = x:[]
inters' sep (x:xs) = x:sep:[] ++ (inters' sep xs)

-- 8
data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ l r)
    | treeHeight l >= treeHeight r = 1 + treeHeight l
    | otherwise = 1 + treeHeight r

-- 9
type Point = (Double, Double)
data Direction = LeftD | StraightD | RightD deriving (Show, Eq)

-- 10
direction :: Point -> Point -> Point -> Direction
direction (x1, y1) (x2, y2) (x3, y3)
    | cross > 0 = LeftD
    | cross == 0 = StraightD
    | cross < 0 = RightD
    where cross = (x2-x1)*(y3-y1)-(x3-x1)*(y2-y1)
    
-- 11
dirList :: [Point] -> [Direction]
dirList (a:b:[]) = []
dirList (a:b:c:ps) = [direction a b c] ++ dirList (b:c:ps)

-- 12
coordSort :: [Point] -> [Point]
coordSort points = sortBy criteria points
    where criteria (x1, y1) (x2, y2)
            | yComp == GT = GT
            | yComp == EQ = compare x1 x2
            | yComp == LT = LT
                where yComp = compare y1 y2

slopeSort :: [Point] -> [Point]
slopeSort ((xp, yp):ps) = (xp, yp):[] ++ sortBy criteria ps
    where criteria (x1, y1) (x2, y2) = compare slope1 slope2
            where slope1 = (y1-yp)/(x1-xp)
                  slope2 = (y2-yp)/(x2-xp)

convexSort :: [Point] -> [Point]
convexSort (p1:p2:p3:[])
    | direction p1 p2 p3 == RightD = (p1:p3:[])
    | otherwise = (p1:p2:p3:[])
convexSort (p1:p2:p3:ps)
    | direction p1 p2 p3 == RightD = convexSort (p1:p3:ps)
    | otherwise = p1:[] ++ convexSort (p2:p3:ps)

grahamScanAlgorithm :: [Point] -> [Point]
grahamScanAlgorithm = convexSort . slopeSort . coordSort
