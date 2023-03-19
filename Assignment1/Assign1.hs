
module Main where
import Language.Haskell.TH.PprLib (sep)
import Data.Char (intToDigit)
-- Exercise 1
-- n l k p = 3 4 9 6
-- If n is fixed, l k p have no meaning ??

addp :: Int->Int
addp 1 = 1
addp p = p + addp (p - 1)

mulk :: Int->Int
mulk 1 = 1
mulk k = addp k * mulk (k - 1)

addl :: Int->Int
addl 1 = 1
addl l = mulk l + addl (l - 1)

muln :: Int->Int
muln 1 = 1
muln n = addl n * muln (n - 1)


-- Res: 0.00 sec, 22,944 bytes use. 
-- Exercise 2
-- S1 = [kiveydewhjusgofiimbbyhwbopvuplwfexresmhtnic]

reverse1 :: String -> String 
reverse1 [] = []
reverse1 a = (++) (reverse1 (tail a)) [head a]

--Exercise 3
--S2 = [kiv], [eydew], [hjusg], [mbbyhwbopvupl], [ofii], [wfexresmhtnic]

concat1:: [String] -> String
concat1 [] = []
concat1 a = (++) (head a) (concat1 (tail a))

--Exercise 4
separate:: [String] -> String
separate [] = []
separate a = concat1 [head a, "-,-", separate (tail a)]


--Exercise 5
-- i = 3, j =4
pascal:: Int->Int->Int
pascal 0 0 = 1
pascal 0 a = 1
pascal i j = 
    if i == j then 1 
    else pascal (i-1) (j-1) + pascal i (j-1)

-- Exercise 6
-- Hilbert
-- Hilbert curve used to sharpen image
-- Hilbert function continuous at every point
-- rotate right
rotr:: Int -> [[Int]]->[[Int]]
rotr n [] = []
rotr n a  = [last (head a), n - 1 - head(head a)]:rotr n (tail a)

-- 2^n
power2:: Int -> Int
power2 0 = 1
power2 n = 2 * power2 (n-1)

--reflex horizontal
reflexy::Int -> [[Int]] -> [[Int]]
reflexy n [] = []
reflexy n a = [n - 1 - head (last a), last (last a)]: reflexy n (init a)

--reverse sequence of coordinates (for easier visualization)
rev:: [[Int]] -> [[Int]]
rev [a] = [a]
rev a = (last a): rev (init a)

--shift by vertical axis
shiftup:: Int -> [[Int]]->[[Int]]
shiftup a [] = []
shiftup a b = [head (head b), a+ last(head b)]: shiftup a (tail b)

--display curve by sequence of coordinates 
hilbert:: Int -> [[Int]]
hilbert 0  = []
hilbert 1  = [[0,0],[0,1],[1,1],[1,0]]
hilbert n  = rev (rotr (power2(n-1)) (hilbert (n-1) )) ++ shiftup (power2 (n-1)) (hilbert (n-1) ) ++ reflexy (power2 n) (rev (rotr (power2 (n-1)) (hilbert (n-1) )) ++ shiftup (power2 (n-1)) (hilbert (n-1)))

--
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
main :: IO ()
main = do
    print "Exercise 1: Result is"
    print(muln 3)
    print "Exercise 2: Reversed list: "
    print (reverse1 "kiveydewhjusgofiimbbyhwbopvuplwfexresmhtnic")
    print "Exercise 3: Concatenation: "
    print (concat1 ["kiv", "eydew", "hjusg", "mbbyhwbopvupl", "ofii", "wfexresmhtnic"])
    print "Exercise 4: "
    print (separate ["kiv", "eydew", "hjusg", "mbbyhwbopvupl", "ofii", "wfexresmhtnic"])
    print "Exercise 5: Pascal number "
    print (pascal 3 4)
    print "Exercise 6: Hilbert curve 7 iterations:"
    writeFile "hilbert.txt" (toStringCustom2 (hilbert 4))
