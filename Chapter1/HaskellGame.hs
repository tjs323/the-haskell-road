import GS

maxInt :: [Int] -> Int
maxInt [] = error "empty list"
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

removeFst :: Int -> [Int] -> [Int]
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