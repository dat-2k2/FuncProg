-- Rook problem
-- Easy version: without any bounce
module Main where
import Language.Haskell.TH.PprLib (sep)
import Distribution.PackageDescription (BuildType(Custom))
import Data.Array (Ix(range))
import Data.Semigroup (diff)
import Control.Monad
import Data.Char (intToDigit)

valid:: [Int] -> [[Int]] -> Bool
valid a [] = True
valid a b = not (head a == head (head b) || last a == last (head b)) && valid a (tail b)

--rook :: Num rook -> Size board -> Blocked cells -> Result
--Notice that a solution of n rooks is a permutation of [sizeboard] 

-- row(col) a is blocked
--tree implementation using record
data CustomTree = Nil | CustomTree{
    value :: Int,
    children :: [CustomTree]
}deriving (Eq,Show)

--Need to derive the IO class before print out
-- Derive class Eq to use equation
mergeCustom:: [[a]] -> [a]
mergeCustom [] = []
mergeCustom x = head x ++ mergeCustom (tail x)

--print all paths from root to leaves
allPaths :: CustomTree ->  [[Int]]
allPaths Nil = [[]]
allPaths x = map ( ++ y) (mergeCustom (map allPaths (children x))) where y = if value x == 0 then [] else [value x]

rangeCustom:: Int -> [Int]
rangeCustom 0 = []
rangeCustom n =  n:rangeCustom (n-1)

-- put one rook on each row
-- Recursive by size of available rows (AFTER PUT ROOK ON POS)
permutation:: Int -> Int -> [Int] ->CustomTree
permutation a b [] = CustomTree b [Nil]
-- map with multiple-arg function
permutation 1 pos availrows = CustomTree pos [Nil]
permutation n pos availrows = CustomTree pos (map (\p -> permutation (n-1) p (filter (\x -> x/= p && x/= pos) availrows)) (filter (/=pos) availrows))

pwithfilter:: Int -> Int -> [Int] -> ([Int] -> Bool) -> CustomTree
pwithfilter a b [] f = if not( f [b, a] ) then Nil else CustomTree b [Nil]
pwithfilter 1 pos rows f = if not( f [pos, 1] ) then Nil else CustomTree pos [Nil]
pwithfilter n pos rows f = if not( f [pos, n] ) then Nil else
    CustomTree pos (map (\p -> pwithfilter (n-1) p (filter (\x -> x/= p && x/= pos) rows) f) (filter (/=pos) rows))
    
rooks:: Int -> CustomTree
rooks n = permutation (n+1) 0 (rangeCustom n)

diffrooks:: Int -> CustomTree
diffrooks n = pwithfilter (n+1) 0  (rangeCustom n) (\a -> (a!!0 /= a!!1))


--print 
inttoString:: Int -> String
inttoString 0 = "0"
inttoString a = if a < 10 then [intToDigit a]  else (inttoString (div a 10)) ++ [intToDigit (mod a 10)]

toStringCustom::[Int] -> String
toStringCustom [] = []
toStringCustom [a] = inttoString a
toStringCustom a = inttoString (head a) ++","++toStringCustom (tail a) 

toStringCustom2:: [[Int]] ->String
toStringCustom2 [] = []
toStringCustom2 [[]] = []
toStringCustom2 [a] = "[" ++ toStringCustom a ++"]"
toStringCustom2 a = "["++(toStringCustom (head a) )++"],"++ (toStringCustom2 (tail a))

f::Int->Int->Int 
f x _ = x

f2:: [Int]
f2 = 0:1:zipWith(+) f2 (tail f2)

main::IO()
main = do
    -- Everything is function
    let leaf1 = CustomTree 1 [Nil]
    let leaf2 = CustomTree 2 [Nil]
    let root = CustomTree 1 [leaf1, leaf2]
    let n = 7
    writeFile "test.txt"  ("[" ++ (toStringCustom2 (filter (\x -> length x == n) (allPaths (rooks n))))++"]")

--ver2: diagonal is blocked. 

