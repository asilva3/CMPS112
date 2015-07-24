--Assignment #3
--Author : Armando Silva asilva3
--Partner: Kevin Thai kjthai
--Time programming together : 6
--Additional individual effort : 2

import Data.List

data BST k v = Empty |
        Node k v (BST k v) (BST k v)
-- 1
val :: BST k v -> Maybe v
val Empty = Nothing
val (Node k v _ _) = Just v

-- 2
size :: BST k v -> Int
size Empty = 0
size (Node k v lc rc) = 1 + size lc + size rc

-- 3
ins :: (Ord k) => k -> v -> BST k v -> BST k v
ins k v Empty = Node k v Empty Empty
ins k v (Node k' v' lc rc)
   | k == k' = Node k v' lc rc
   | k < k'  = Node k' v' (ins k v lc) rc
   | k > k'  = Node k' v' lc (ins k v rc)
   ;

-- 4
instance (Show v) => Show (BST k v) where
 show Empty = ""
 show (Node k v left right) = "(" ++ show left ++ show v ++ show right ++ ")"
 
data JSON = JStr String
          | JNum Double
          | JArr [JSON]
          | JObj [(String, JSON)]

-- 5 Made a colon function to add in the colons
instance Show JSON where
    show (JStr s) = show s
    show (JNum n) = show n
    show (JArr a) = "[" ++ intercalate "," (map show a) ++ "]"
    show (JObj o) = "{" ++ intercalate "," (map colon o) ++ "}"
	
colon :: (String, JSON) -> String
colon (x,y) = show x ++ ":" ++ show y

-- 6
class Json a where
  toJson :: a -> JSON
  fromJson :: JSON -> a
	
instance Json Double where 
   toJson = JNum
   fromJson (JNum x) = x
   
instance (Json a) => Json [a] where
  toJson x = JArr (map toJson x)
  fromJson (JArr x) = (map fromJson x)


	
