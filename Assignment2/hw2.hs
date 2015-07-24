--Assignment #1
--Author : Armando Silva asilva3
--Partner : Kevin Thai kjtahi
--Time programming together: 8
--Additional individual effor: 2

--Problem #1, Using recursion
myFoldl :: (a->b->a) -> a -> [b] -> a
myFoldl f a [] = a
myFoldl f a (x:xs) = myFoldl f(f a x) xs

--Problem #2, Using the standard foldl, function behaves like reverse
myReverse :: [a]->[a]
myReverse = foldl(\acc x -> x:acc) []

--Problem #3, Using the standar foldl write a function that
--behaves just like the standard foldr.
myFoldr :: (a->b->b) -> b-> [a] -> b
myFoldr f a [] = a
myFoldr f a x = foldl(\acc b y -> acc(f b y)) id x a

--Problem #4, Using the standard foldr, write a function
--which behaves just like the standar foldl
myFoldl2 :: (a->b->a) ->a ->[b] -> a
myFoldl2 f z [] = z
myFoldl2 f z x = foldr(\acc y x -> y(f x acc))id x z 

--Problem #5, write a function which returns true if the provided
--character is in the range 'A' to 'Z'
isUpper :: Char -> Bool
isUpper x =  x `elem` ['A'..'Z']


--Problem #6 Using the standard filter, write a function
--which returns only the capital letters of the provided string
onlyCapitals1 :: String -> String
onlyCapitals1 x = filter(isUpper) x 

--Problem #7 Using List comprehension write a function
--which returns only the capital letters of the provided string.
onlyCapitals2 :: String -> String
onlyCapitals2 x = [x | x <- x, isUpper x]

--Problem #8 Using recursion, write a function
--which returns only the captial letters of the provided string
onlyCapitals3 :: String -> String
onlyCapitals3 "" = ""
onlyCapitals3 (x:xs) 
	| isUpper x = x : onlyCapitals3 xs
	| otherwise = onlyCapitals3 xs

--Problem #9 Write a function, which returns a tuple with the 
--quotient and the remainder of an integer division of the 
--provided two numbers:
divRemainder :: Int -> Int -> (Int, Int)
divRemainder x y = ((x `div` y) ,(x `mod` y))

--Problem #10 Write a function, which returns the sum of digits of the 
--given integer using recusrion, guard and learned 9.
digitSum :: Int -> Int
digitSum x 
    | x <= 9 = x
    |otherwise = digitSum(fst (divRemainder x 10)) + snd(divRemainder x 10)

--Problem #11 which takes a string of digits and spells out the number
--as string in English
sayOnes :: Integer -> String
sayOnes x 
	| x == 1 = "One"
	| x == 2 = "Two"
	| x == 3 = "Three"
	| x == 4 = "Four"
	| x == 5 = "Five"
	| x == 6 = "Six"
	| x == 7 = "Seven"
	| x == 8 = "Eight"
	| x == 9 = "Nine"
	| x == 10 = "Ten"
	| x == 11 = "Eleven"
	| x == 12 = "Twelve"
	| x == 13 = "Thirteen"
	| x == 15 = "Fifteen"
	| x == 18 = "Eighteen"
	| x > 10 && x<20 = sayOnes(x `mod` 10) ++ sayTen(x `div` 10)
	| x >= 20 && x < 100 = sayTen(x `div` 10) ++ sayOnes(x `mod` 10)
	| x >= 100  && x < 1000 = sayOnes(x `div` 100) ++ " hundred " ++ sayTen((x `mod` 100) `div` 10) ++ sayOnes(x `mod` 10)
	| otherwise = ""

sayTen :: Integer -> String
sayTen x
	| x == 1 = "teen"
	| x == 2 = "Twenty "
	| x == 3 = "Thirty "
	| x == 4 = "Fourty "
	| x == 5 = "Fifty "
	| x == 6 = "Sixty "
	| x == 7 = "Seventy "
	| x == 8 = "Eighty "
	| x == 9 = "Ninty "
	| otherwise = " "

sayNum :: Integer -> String 
sayNum x
    | (x >= 10^63 && x < 10^66)= sayOnes(x `div` 10^63) ++ " vigintillion " ++ sayNum (x `mod` 10^63)
    | (x >= 10^60 && x < 10^63)= sayOnes(x `div` 10^60) ++ " novemdecillion " ++ sayNum (x `mod` 10^60)
    | (x >= 10^57 && x < 10^60)= sayOnes(x `div` 10^57) ++ " octodecillion  " ++ sayNum (x `mod` 10^57)
    | (x >= 10^54 && x < 10^57)= sayOnes(x `div` 10^54) ++ " septendecillion " ++ sayNum (x `mod` 10^54)
    | (x >= 10^51 && x < 10^54)= sayOnes(x `div` 10^51) ++ " sexdecillion " ++ sayNum (x `mod` 10^51)
    | (x >= 10^48 && x < 10^51)= sayOnes(x `div` 10^48) ++ " quindecillion  " ++ sayNum (x `mod` 10^48)
    | (x >= 10^45 && x < 10^48)= sayOnes(x `div` 10^45) ++ " quattuordecillion " ++ sayNum (x `mod` 10^45)
    | (x >= 10^42 && x < 10^45)= sayOnes(x `div` 10^42) ++ " tredecillion " ++ sayNum (x `mod` 10^42)
	| (x >= 10^39 && x < 10^42)= sayOnes(x `div` 10^39) ++ " duodecillion " ++ sayNum (x `mod` 10^39)
    | (x >= 10^36 && x < 10^39)= sayOnes(x `div` 10^36) ++ " undecillion " ++ sayNum (x `mod` 10^36)
    | (x >= 10^33 && x < 10^36)= sayOnes(x `div` 10^33) ++ " decillion " ++ sayNum (x `mod` 10^33)
    | (x >= 10^30 && x < 10^33)= sayOnes(x `div` 10^30) ++ " nonillion " ++ sayNum (x `mod` 10^30)
    | (x >= 10^27 && x < 10^30)= sayOnes(x `div` 10^27) ++ " octillion " ++ sayNum (x `mod` 10^27)
    | (x >= 10^24 && x < 10^27)= sayOnes(x `div` 10^24) ++ " septillion " ++ sayNum (x `mod` 10^24)
    | (x >= 10^21 && x < 10^24)= sayOnes(x `div` 10^21) ++ " sextillion " ++ sayNum (x `mod` 10^21)
    | (x >= 10^18 && x < 10^21)= sayOnes(x `div` 10^18) ++ " quintillion " ++ sayNum (x `mod` 10^18)
    | (x >= 10^15 && x < 10^18)= sayOnes(x `div` 10^15) ++ " quadrillion " ++ sayNum (x `mod` 10^15)
    | (x >= 10^12 && x < 10^15)= sayOnes(x `div` 10^12) ++ " trillion " ++ sayNum (x `mod` 10^12)
    | (x >= 10^9 && x < 10^12)= sayOnes(x `div` 10^9) ++ " billion " ++ sayNum (x `mod` 10^9)
    | (x >= 10^6 && x < 10^9) = sayOnes(x `div` 10^6) ++ " million " ++ sayNum (x `mod` 10^6)
	| (x >= 10^3 && x < 10^6) = sayOnes(x `div` 10^3) ++ " thousand " ++ sayNum(x `mod` 10^3)
	| otherwise = sayOnes(x)