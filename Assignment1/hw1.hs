--Assignment #1
--Author : Armando Silva asilva3 
--Partner: Kevin Thai kjthai
--Time programming together : 6
--Additional individual effort : 2


--imported to use ord to change char to int
import Data.Char

--2 Flips the first and last name
citeAuthor :: String -> String -> String
citeAuthor fname lname = lname ++ ", " ++ fname

--3 Gets the initials
initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
    where(f:_) = firstname
         (l:_) = lastname

--4 Returns the title
title :: (String, String, Int) -> String
title ( _, y, _) = y

--5 Formats the author title and year
citeBook :: (String, String, Int) -> String
citeBook (author, title, year) = title ++ " ("  ++ author ++ ", " ++ show year ++ ")"

--6 Recursive citeBook for multiple
bibliography_rec :: [(String, String, Int)] -> String
bibliography_rec [] = ""
bibliography_rec (x:xs) = citeBook x ++ "\n" ++ bibliography_rec xs

--7 Use foldl to do what 6 did
bibliography_fold :: [(String, String, Int)] -> String
bibliography_fold xs = foldl(\acc x -> acc  ++ citeBook x ++ "\n")[] xs

--8 Gets the average year
-- Returns the third element or year
year :: (String, String , Int) -> Int
year (_,_,z) = z

--Recursively adds the years
addYear :: [(String, String, Int)] -> Int
addYear [] = 0
addYear (x:xs) = (year x + addYear xs)

-- Puts the years into a list
divYear :: [(String, String, Int)] -> [Int]
divYear xs = [ z | (_,_,z) <- xs]

-- Divide to get the average
averageYear :: [(String, String, Int)] -> Int
averageYear x = addYear x `div` length(divYear x)


--9 Looks for the references
-- Use filter to look for the brackets
filter' :: String -> String
filter' x = (filter(`elem` ['[',']'] ++ " " )  x )
	  
references :: String -> Int
references x =  length(words(filter' x))

--10 Inserts citeBook into references
-- Gets the index of citeBook
reCite :: [(String, String, Int)] -> Int -> String
reCite x y = citeBook(x !! y)

--parse the string to find the references
parser :: [(String, String, Int)] -> String -> String
parser book (x:y:z) 
          | x == '[' =  reCite book (ord y - 49)
		  | otherwise = x:y:z
parser x y = y

--returns the changed string          
citeText :: [(String, String, Int)] -> String -> String
citeText x y = unwords (map (parser x) (words y))

