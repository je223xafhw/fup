import Prelude hiding (hd, tl)


square:: Float -> Float
square x = x^2

fac:: Integer -> Integer
fac n = if n==0 then
    1
    else
        n*fac (n-1)

-- -------------------------------------------------------------------------- --
--                          fib function that is bad,                         --
-- -------------------------------------------------------------------------- --
fib:: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- -------------------------------------------------------------------------- --
--                           fib funciton thats good                          --
-- -------------------------------------------------------------------------- --
fib2:: Integer -> Integer
-- ----------------------------- _ is a wildcard ---------------------------- --
fib2 n = fib2' n 0 1
    where
        fib2':: Integer -> Integer -> Integer -> Integer
        fib2' 0 fibn _ = fibn
-- ----------------------------- fibnp1 = fibn+1 ---------------------------- --
        fib2' n fibn fibnp1 = fib2' (n-1) fibnp1 (fibn + fibnp1)

-- -------------------------------------------------------------------------- --
--                              prim number check                             --
-- -------------------------------------------------------------------------- --
isPrime :: Integer -> Bool
isPrime n = n > 1 && checkDiv (n `div` 2)
    where
        checkDiv :: Integer -> Bool
        checkDiv m = m == 1 || n `mod` m /= 0 && checkDiv (m-1)

s1 :: String -- oder [Char]
s1 = "test"

hd :: [a] -> a
hd (x:_) = x

tl :: [a] -> [a]
tl (_:xs) = xs



main :: IO ()
main = do
    -- print(isPrime 35)
    print(hd [1,2,3,32])
