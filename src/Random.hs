module Random where

import Data.Vector as V hiding ((++))

type GenState = (Int, Int)

gen :: Int -> Int -> Int -> Int -> GenState -> GenState
gen a b c m (x1, x2) = let x3 = (a*x1 + b*x2 + c) `mod` m
                         in x3 `seq` (x2, x3)

period :: (Eq a) => (a -> a) -> a -> (Int, a)
period f x0 = z `seq` (go2 (f z) 1, z)
  where
    z = go1 (f x0) (f (f x0))
    go1 x y | x==y = x
            | otherwise = go1 (f x) (f (f y))
    go2 x n | x==z = n
            | otherwise = go2 (f x) (n+1)

preperiod :: (Eq a) => (a -> a) -> a -> Int
preperiod f x0 = z `seq` go1 x0 z 0
  where
    z = go2 x0 0 (fst $ (period f x0))
    go1 x y n   | x==y      = n
                | otherwise = go1 (f x) (f y) (n+1)
    go2 x n lim | n==lim    = x
                | otherwise = go2 (f x) (n+1) lim
            
quality :: ((Int, Int) -> (Int, Int)) -> (Int, Int) -> Int -> Double
quality f (x1, x2) m = qsum $ go1 (x1, x2) 1 400 m (V.replicate 20 0)
  where
    cnt x = ((fromIntegral x) - 20.0)*((fromIntegral x) - 20.0)/400.0
    qsum arr | (V.length arr) == 0 = 0
             | otherwise = (cnt (V.head arr)) + qsum (V.tail arr)
    get x1 m = ((x1 `mod` m)*20) `div` m
    go1 (x1, x2) n lim m arr | n==lim    = arr
                             | n==1      = go1 (f (x1, x2)) (n+1) lim m (arr // [((get x1 m), (arr!(get x1 m)) + 1)])
                             | otherwise = go1 (f (x1, x2)) (n+1) lim m (arr // [((get x2 m), (arr!(get x2 m)) + 1)])

research :: ((Int, Int) -> (Int, Int)) -> (Int, Int) -> Int -> String                                                                         
research gener x0 d = "p: "  ++ (show $ fst $ period gener x0) ++ "\n" ++
                      "pp: " ++ (show $ preperiod gener x0) ++ "\n" ++
                      "q: "  ++ (show $ quality gener x0 d)