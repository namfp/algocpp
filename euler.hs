problem1 = sum [x | x <- [1 .. 999], x `rem` 3 == 0 || x `rem` 5 == 0]

fibs = map fst $ iterate (\ (a, b) -> (b, a + b)) (0,1)

factors :: Integer -> [Integer]
factors n
        | n == 1 = [1]
        | otherwise = f n 2 [1]
            where f n i l
                    | n `rem` i == 0 = f (n `div` i) i (i:l)
                    | (fromIntegral i) <= n = f n (i + 1) l
                    | otherwise = l

problem3 = head . factors

p31 = g 10 [200,100,50,20,10,5,2,1]
  where
    g 0 _  = [[]]    -- exactly one way to get 0 sum, with no coins at all
    g n [] = []      -- no way to sum up no coins to a non-zero sum
    g n coins@(c:rest) 
      | c <= n = map (c:) (g (n-c) coins)  -- with the top coin
                  ++ g n rest
      | otherwise = g n rest

ways [] = 1 : repeat 0
ways (coin:coins) = n
    where n = zipWith (+) (ways coins) (replicate coin 0 ++ n)


problem_31 = ways [1,2,5,10,20,50,100,200] !! 200

dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

repli [] _= []
repli (x:xs) n = take n (repeat x) ++ (repli xs n)

dropEvery l n = f l n 1
    where 
        f [] _ _ = []
        f (x:xs) n k =  if (k == n)
        then (f xs n 1)
        else x:(f xs n (k + 1))

split l n = getResult r
    where
        r = foldl f ([], [], 0, n) l 
            where f (left, right, k, n) x
                    | k < n = ((x:left), right, k + 1, n)
                    | otherwise = (left, (x:right), k + 1, n)
        getResult (x, y, _, _) = (x, y)

slice [] _ _ = []
slice (x:xs) a b
    | b < 1 = []
    | a > 1 = slice xs (a - 1) (b - 1)
    | a <= 1 = x:(slice xs 0 (b - 1))
    | b >= 1 = [x]

rotate l n = getResult r
    where
        index = if (n < 0)
                then (length l) + n
                else n
        f (temp, result, k, i) x
            | k <= i = (x:temp, result, (k + 1), i)
            | otherwise = (temp, x:result, (k + 1), i)
        r = foldl f ([], [], 1, index) l
        getResult (x, y, _, _) = reverse y ++ reverse x
       
removeAt l n = getResult r
                where 
                    f (removed, rest, k, n) x
                        | k == n = (x:removed, rest, k + 1, n)
                        | otherwise = (removed, x:rest, k + 1, n) 
                    r = foldl f ([], [], 1, n) l
                    getResult (x, y, _, _) = (reverse x, reverse y)

insertAt e l 1 = e:l 
insertAt e (x:xs) n = x: insertAt e xs (n - 1)


range a b = 
    if (b < a)
        then error "not good"
    else
        if (a == b) 
        then [a] 
        else a: range (a + 1) b

ins a [] = [[a]]
ins a l@(x:xs) = getResult r
    where
        f (left, right, result, a) x = (    
                                        left ++ [x], 
                                        tail right, 
                                        (left ++ [a] ++ right):result, 
                                        a)
        r =  foldl f ([], l, [], a) l
        getResult (_, _, r, _) = (l ++ [a]):r


myPermute [] = [[]]
myPermute (x:xs) = concatMap (ins x) (myPermute xs)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n - 1) xs)) ++ (combinations n xs)

findRest [] l2 = l2
findRest _ [] = []
findRest (x:xs) l2 =    let
                            rest = findRest xs l2
                        in 
                            filter (\ e -> e /= x) rest

findRestAll l1 l2 = foldr f l2 l1
                    where
                        f el1 l = findRest el1 l


--group (x:xs) groupElems = let
--                            rest = group xs groupElems
--                            restElems g l= foldr f [] l
--                                where 
