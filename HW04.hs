{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P []) (P []) = True
    (==) (P []) (P (t:ts)) = t == 0 && P [] == P ts
    (==) (P s) (P []) = P [] == P s
    (==) (P (s:ss)) (P (t:ts)) = s == t && P ss == P ts 
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P arr) = showHelper 0 arr
         where showHelper :: (Num a, Eq a, Show a) => a -> [a] -> [Char]
               showHelper _ [] = "0"
               showHelper pos (l:ks)
                          | l == 0 = showHelper (pos + 1) ks
                          | ks == [] = showTerm pos l
                          | otherwise = showHelper (pos + 1) ks ++ " + " ++ showTerm pos l
               
               showTerm 0 coeff = show(coeff)
               showTerm _ 0 = ""
               showTerm 1 coeff = show(coeff) ++ "x"
               showTerm pos coeff = show(coeff) ++ "x^" ++ show(pos)

-- Exercise 4 -----------------------------------------
zipWithAll :: Num a => (a -> a -> a) -> [a] -> [a] -> [a]
zipWithAll _ s [] = s
zipWithAll _ [] t = t
zipWithAll f (s:ss) (t:ts) = (f s t) : zipWithAll f ss ts

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P []) (P t) = P t
plus (P s) (P []) = P s
plus (P s) (P t) = P (zipWithAll (+) s t)

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P s) (P t) = sum $ map P $ timesHelper 0 s t
      where timesHelper :: Num a => Int -> [a] -> [a] -> [[a]]
            timesHelper _ [] _ = [] 
            timesHelper _ _ [] = []
            timesHelper i (k:ks) l = [map (*k) $ take i (repeat 0) ++ l] ++ (timesHelper (i+1) ks l)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P p) = P $ map negate p
    fromInteger i = P [fromIntegral i]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P s) t = sum $ zipWith (evalTerm t) s [0,1 ..]
       where evalTerm :: Num b  => b -> b -> Integer -> b
             evalTerm v i j
                      | j == 0 = i
                      | otherwise = i*(v^j)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 1 p = deriv p
    nderiv n p = nderiv (n-1) $ deriv p

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P s) = P $ tail $ zipWith derivTerm s [0,1 ..]
          where derivTerm :: Num b => b -> Integer -> b
                derivTerm i j
                          | j == 0 = 0
                          | j == 1 = i * k
                          | otherwise = i * k
                                      where k = fromIntegral j
