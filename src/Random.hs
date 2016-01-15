module Random where

type GenState = (Int, Int)
type Generator = (Int, Int, Int, Int, Int, Int)

genNext :: Int -> Int -> Int -> Int -> GenState -> GenState
genNext a b m (x1, x2) = let x3 = (a*x1 + b*x2 + c) `mod` m
                         in x3 `seq` (x2, x3)

countper :: (Eq a) => (a -> a) -> a -> (Int, a)
countper f x0 = z `seq` (go2 (f z) 1, z)
  where
    z = go1 (f x0) (f (f x0))
    go1 x y | x==y = x
            | otherwise = go1 (f x) (f (f y))
    go2 x n | x==z = n
            | otherwise = go2 (f x) (n+1)

countpreper :: (Eq a) => (a -> a) -> a -> Int
countpreper f x0 = go2 x0 (go1 x0 1) 1
  where
    per = fst . countper a f x0
    go1 x n   | n==per = x
              | otherwise = go1 (f x) (n+1)
    go2 x y n | x==y = n
              | otherwise = go2 (f x) (f y) (n+1)

countk :: Generator -> Int -> Int -> (Int, Int) -> Int
countk gener k (-1) (_, _) = 0
countk (a, b, c, m, x0, x1) k count (x2, x3) = (check x1 20 m k) + countk(a, b, c, m, x0, x1) k (count-1) (gen (a, b, c, m, x0, x1) (x2, x3))
        where
        check x1 i m k | ((x1*i) `div` m == k) = 1
                       | otherwise             = 0

quality :: Generator -> Int -> Double
quality gener (-1) = 0
quality (a, b, c, m, x0, x1) count = (((fromIntegral (countk (a, b, c, m, x0, x1) count 400 (x0, x1))) - 20.0)/20.0)**2 +
                                     (quality (a, b, c, m, x0, x1) (count-1))
