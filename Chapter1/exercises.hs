import GS
import Data.Char

-- Chapter 1.5

maxInt :: [Int] -> Int
maxInt [] = error "empty list"
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

removeFst :: Ord a => a -> [a] -> [a]
removeFst m [] = []
removeFst m (x:xs) = if m == x then xs else (x:(removeFst m xs))

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs

average :: [Int] -> Rational
average [] = error "empty list"
average xs = (/) (toRational (sum xs)) (toRational (length xs))

count :: Char -> String -> Int
count c [] = 0
count c (x:xs) 	| c == x = 1 + count c xs
				| otherwise = count c xs

repeatChar :: Char -> Int -> String
repeatChar c 0 = []
repeatChar c n = c : (repeatChar c (n-1))

blowup' :: String -> Int -> String
blowup' [] n = []
blowup' (c:cs) n = (repeatChar c n) ++ (blowup' cs (n+1))

blowup :: String -> String
blowup xs = blowup' xs 1

lowerString :: String -> String
lowerString [] = []
lowerString (x:xs) = (toLower x) : (lowerString xs)

mnmStr :: [String] -> String
mnmStr [] = error "empty list"
mnmStr [x] = x
mnmStr (x:xs) = min x (mnmStr xs)

srtString :: [String] -> [String]
srtString [] = []
srtString xs = m : (srtString (removeFst m xs)) where m = mnmStr xs

subString :: String -> String -> Bool
subString str1 str2 | (length str1) > (length str2) = False
					| (length str1) == (length str2) = str1 == str2
					| otherwise = (prefix str1 str2) || (subString str1 (tail str2))


-- Chapter 1.8

-- Exercise 1.20
lengths :: [[a]] -> [Int]
lengths = map length

-- Exercise 1.21
sumLengths :: [[a]] -> Int
sumLengths = sum . lengths


