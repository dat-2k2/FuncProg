-- Rook problem
-- Easy version: without any bounce
module Main where
import Language.Haskell.TH.PprLib (sep)
import Distribution.PackageDescription (BuildType(Custom))
import Data.Array (Ix(range))

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
allPaths :: CustomTree -> [[Int]]
allPaths Nil = [[]]
allPaths x = map (y ++) (mergeCustom (map allPaths (children x))) where y = if value x == 0 then [] else [value x]

rangeCustom:: Int -> [Int]
rangeCustom 0 = []
rangeCustom n = n: rangeCustom (n-1)

-- put one rook on each row
-- Recursive by size of available rows
permutationwithfilter:: Int -> Int -> [Int]->CustomTree
permutationwithfilter a b [] = CustomTree b [Nil]
-- map with multiple-arg function
permutationwithfilter 1 pos rows = CustomTree pos [Nil]
permutationwithfilter n pos rows = CustomTree pos (map (\p -> permutationwithfilter (n-1) p (filter (\x -> x/= p && x/= pos) rows)) (filter (/=pos) rows))

rooks:: Int -> CustomTree
rooks n = permutationwithfilter (n+1) 0 (rangeCustom n)

main::IO()
main = do
    -- Everything is function
    let leaf1 = CustomTree 1 [Nil]
    let leaf2 = CustomTree 2 [Nil]
    let root = CustomTree 1 [leaf1, leaf2]
    print (allPaths (rooks 10))
--ver2: diagonal is blocked. 

