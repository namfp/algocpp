import System.Random

put :: Double -> Double -> Double
put k s = max (k - s) 0.0


call :: Double -> Double -> Double
call k s = max (s - k) 0.0


euroBackward :: Double -> Double -> Double -> Double -> Double 
                    -> (Double, Double) -> (Double, Double) -> (Double, Double)
euroBackward k p dt r u x y = ((((fst x) * p + ((fst y) * (1 - p))) * exp (-r * dt)), 
                            (snd x) / u)


americanBackward :: (Double -> Double -> Double) 
                        -> Double -> Double -> Double -> Double -> Double
                        -> (Double, Double) -> (Double, Double) -> (Double, Double)
americanBackward payOff k p dt r u x y = 
    let underlyingPrice = (snd x) / u
        optionPriceWithoutExercice = (((fst x) * p + ((fst y) * (1 - p))) * exp (-r * dt))
        exercicePayOff = payOff k underlyingPrice
    in 
        ((max optionPriceWithoutExercice exercicePayOff), underlyingPrice)



price ::    (Double -> Double -> Double -> Double -> Double 
                -> (Double, Double) -> (Double, Double) 
                -> (Double, Double))
            -> (Double -> Double -> Double) 
            -> Double -> Double -> Double -> Double -> Integer -> Double 
            -> Double -> [(Double, Double)]
price backWardFunc payOff r q sigma s0 nt t k = 
    let dt = t / (fromIntegral nt :: Double)
        a = exp $ (r - q) * dt
        d = exp (-sigma * sqrt dt)
        u = exp (sigma * sqrt dt)
        p = (a - d) / (u - d)
        s :: Integer -> Integer -> Double
        s fu fv = s0 * u^fu * d^fv
        finalPrices = zipWith (\x y -> (payOff k (s x y), s x y) )
            [nt, nt-1.. 0] [0..nt]
        backward :: [(Double, Double)] -> [(Double, Double)]
        backward ((x, y):[]) = [(x, y)]
        backward prices = backward (zipWith
            (\ x y -> backWardFunc k p dt r u x y) 
            prices (tail prices))
    in
        backward finalPrices

americanOptionPut  = price (americanBackward put) put
americanOptionCall  = price (americanBackward call) call
europeanOptionPut = price euroBackward put
europeanOptionCall = price euroBackward call



boxMuller :: Floating a => a -> a -> (a,a)
boxMuller u1 u2 = (r * cos t, r * sin t) where r = sqrt (-2 * log u1)
                                               t = 2 * pi * u2


boxMullers :: Floating a => [a] -> [a]
boxMullers (u1:u2:us) = n1:n2:boxMullers us 
    where 
        (n1,n2) = boxMuller u1 u2
boxMullers _          = [] 


randomList :: (Random a) => Int -> [a]
randomList seed = randoms (mkStdGen seed)

normal :: (RandomGen g, Random a, Floating a) => g -> (a,g)
normal g0 = (fst $ boxMuller u1 u2, g2)
  -- While The Haskell 98 report says "For fractional types, the
  -- range is normally the semi-closed interval [0,1)" we will
  -- specify the range explicitely just to be sure.
  where
     (u1,g1) = randomR (0,1) g0
     (u2,g2) = randomR (0,1) g1

main :: IO ()
main = do print $ take 10 (randomList 42 :: [Int])