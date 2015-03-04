findLass [] = Nothing
findLass [x] = Just x
findLass (x:xs) = findLass(xs)

findLastButOne [] = error "nothing"
findLastButOne (x:[]) = error "nothing"
findLastButOne (x:y:[]) = x
findLastButOne (x:xs) = findLastButOne xs


elemenAt [] _ = error "no element"
elemenAt (x:xs) k = if (k <= 0)
                    then error "no element"
                    else if (k == 1)
                        then x
                        else elemenAt xs (k - 1)

myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

isPalindrome l = l == (myReverse l)

data NestedList a = Elem a | List [NestedList a]
flatten (Elem x) = [x]
flatten (List l) = concatMap flatten l

compress [] = []
compress (x:[]) = [x]
compress (x:y:xs) = if (x == y)
                        then compress (x:xs)
                        else x:(compress (y:xs))

pack l =    let result = foldl f ([], []) l
                                where
                                    f ([], r) y = ([y], r)
                                    f (rest@(x:xs), r) y = if (x == y)
                                                            then (y:x:xs, r)
                                                                else ([y], r ++ [rest])
            in snd result ++ [(fst result)]

encode [] = []
encode (x:xs) = let (first, rest) = span (==x) xs
                in
                    (length first + 1, x) : (encode rest)

data Encode a = Multiple Int a | Single a deriving(Show)

encodeModified [] = []
encodeModified (x:xs) = let (first, rest) = span (==x) xs
                        in
                            if ((length first) == 0)
                            then (Single x): (encodeModified rest)
                            else (Multiple ((length first) + 1) x) : (encodeModified rest)

ins y [] = [[y]]
ins y l@(x:xs)  = f [] y (x:xs)
    where
        f first y [] = [first ++ [y]]
        f first y l@(x:xs) = (first ++ [y] ++ l) : (f (first ++ [x]) y xs)


permutation [] = [[]]
permutation l@(x:xs) = concatMap (ins x) (permutation xs)



-- map (\y -> x:y) (permutation xs)