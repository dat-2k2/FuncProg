
module Main where
import Language.Haskell.TH.PprLib (sep)
-- Exercise 1
-- n l k p = 3 4 9 6
-- If n is fixed, l k p have no meaning ??
addEx :: Int->Int->Int
addEx x y = x+y

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

