import Prelude hiding (map, flip, filter, sum, foldr, id, reverse, foldl)

insert :: a -> [a] -> [[a]]
insert x []     = [[x]]
insert x (y:ys) = (x : y : ys) : (map (y:) (insert x ys))
--insert x (y:ys) = (x : y : ys) : addToFront y (insert x ys)
--   where addToFront :: a -> [[a]] -> [[a]]
--         addToFront x []       = []
--         addToFront x (ys:yss) = (x:ys) : addToFront x yss 

-- falsch: map :: a -> b -> [a] -> [b]
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

add :: Int -> Int -> Int
add x y = x + y

inc :: Int -> Int
inc = add 1

map1 = \f xs -> case xs of
                  []   -> []
                  y:ys -> f y : map1 f ys

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

filter :: (a -> Bool) -> [a] -> [a]
filter p []     = []
--filter p (x:xs) = if p x then x:filter p xs
--                         else filter p xs
filter p (x:xs) | p x       = x : filter p xs
                | otherwise = filter p xs
                         
sum :: [Int] -> Int
--sum []     = 0
--sum (x:xs) = x + sum xs
sum xs = foldr (+) 0 xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f e []     = e
foldr f e (x:xs) = x `f` (foldr f e xs)

--map2 f xs = foldr (\x res -> f x : res) [] xs
map2 f = foldr (\x -> (f x :)) []

filter1 :: (a -> Bool) -> [a] -> [a]
--filter1 p = foldr (\x res -> if p x then x:res else res) []
filter1 p = foldr (\x -> if p x then (x:) else id) []

id :: a -> a
id x = x

reverse = rev' []
  where rev' acc []     = acc
        rev' acc (x:xs) = rev' (x:acc) xs
        
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f e []     = e
foldl f e (x:xs) = foldl f (f e x) xs

sumL = foldl (+) 0

--reverseL = foldl (\res x -> x : res) []
reverseL = foldl (flip (:)) []

data Tree a = Empty | Node (Tree a) a (Tree a)
  deriving Show

-- Empty :: Tree a
-- e     :: b
-- Node  :: Tree a -> a -> Tree a -> Tree a
-- f     :: b      -> a -> b      -> b
foldTree :: b -> (b -> a -> b -> b) -> Tree a -> b
foldTree e n Empty = e
foldTree e n (Node tl x tr) = n (foldTree e n tl) x (foldTree e n tr)

sumTree = foldTree 0 (\resl x resr -> resl + x + resr)



