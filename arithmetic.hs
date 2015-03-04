isPrime :: (Integral a) => a -> Bool
isPrime n | n < 4 = n > 1
isPrime n = all ((/=0).mod n) $ 2:3:[x + i | x <- [6,12..s], i <- [-1,1]]
            where s = floor $ sqrt $ fromIntegral n

myGCD a 0 = a
myGCD a b = myGCD b (a `rem` b)

coPrime a b = (myGCD a b) == 1

totient 1 = 1
totient m = length [r | r <- [1 .. m - 1], coPrime m r]

primeFactors n = f 2 [] n n
    where
        f i l n nOrigin
            | i >= nOrigin = l
            | (isPrime i) && (n `rem` i == 0) = f i (i:l) (n `div` i) nOrigin
            | otherwise = f (i + 1) l n nOrigin

primeFactorsMul n = f 2 [] n (0, 0) n
    where
        f i l n (current, mult) nOrigin
            | i >= nOrigin = if (current /= 0) then (current, mult):l else l
            | (isPrime i) && (n `rem` i == 0) = f i l (n `div` i) (i, mult + 1) nOrigin
            | otherwise = f (i + 1) nextL n (0, 0) nOrigin
                where
                    nextL = if (current /= 0) then (current, mult):l else l

totientImproved m = foldr1 (*) $ map f $ primeFactorsMul m
    where
        f (x, y) = (x - 1) * x ^ (y - 1)

primesR a b = [n | n <- [a..b], (isPrime n)]

goldbach n = head [(i, n - i) | i <- [1 .. n - 1], (isPrime i), (isPrime (n - i))]

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

symetric Empty = True
symetric (Branch x Empty Empty) = True
symetric (Branch x tree1 tree2) = tree1 == (symetric' tree2)
    where
        symetric' Empty = Empty
        symetric' (Branch x left right) = (Branch x (symetric' right) (symetric' left))

ins x Empty = Branch x Empty Empty
ins x (Branch y left right)
    | x == y = Branch y left right
    | x > y = Branch y left (ins x right)
    | x <y = Branch y (ins x left) right

construct [] = Empty
construct l@(x:xs)= foldl (\tree x -> ins x tree) Empty l

countLeaves Empty = 0
countLeaves (Branch x left right) = 1 + (countLeaves left) + (countLeaves right)

internals Empty = 0
internals (Branch x left right)
    | left == Empty && right == Empty = 0
    | otherwise = 1 + (internals left) + (internals right)