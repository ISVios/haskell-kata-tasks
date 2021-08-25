module Stream where

import Control.Arrow
import Control.Applicative

import Stream.Internal

-- Defined in Stream.Internal:
--     data Stream a = a :> Stream a
--     infixr :>

-- | Get the first element of a stream.
headS :: Stream a -> a
headS (a :> xs) = a

-- | Drop the first element of a stream.
tailS :: Stream a -> Stream a
tailS (x :> xs) = xs


-- {{{ Stream constructors

-- | Construct a stream by repeating a value.
repeatS :: a -> Stream a
repeatS a = a :> repeatS a

-- | Construct a stream by repeatedly applying a function.
iterateS :: (a -> a) -> a -> Stream a
iterateS f x = x :> iterateS f (f x)

-- | Construct a stream by repeating a list forever.
cycleS :: [a] -> Stream a
cycleS xs = foldl (\f s -> s :> f) (cycleS xs) xs

-- | Construct a stream by counting numbers starting from a given one.
fromS :: Num a => a -> Stream a
fromS = iterateS (+1)

-- | Same as 'fromS', but count with a given step width.
fromStepS :: Num a => a -> a -> Stream a
fromStepS x s = iterateS (+s) x

-- }}}


-- | Fold a stream from the left.
foldrS :: (a -> b -> b) -> Stream a -> b
foldrS f (x :> xs) = f x y
  where
    y = foldrS f xs

-- | Filter a stream with a predicate.
filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p  (x :> xs) | p x       = x :> (filterS p xs)
                     | otherwise = filterS p xs

-- | Take a given amount of elements from a stream.
takeS :: Int -> Stream a -> [a]
takeS 0 _         = []
takeS i (x :> xs) = x : takeS (pred i) xs

-- | Drop a given amount of elements from a stream.
dropS :: Int -> Stream a -> Stream a
dropS 0 xs     = xs
dropS n (x :> xs) = dropS (pred n) xs

-- | Do take and drop simultaneous.
splitAtS :: Int -> Stream a -> ([a], Stream a)
splitAtS i s = (takeS i s, dropS i s)

-- | Combine two streams with a function.
zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS f (x :> xs) (y :> ys) = f x y :> zipWithS f xs ys 

zipS :: Stream a -> Stream b -> Stream (a, b)
zipS = zipWithS (,)

instance Functor Stream where
    --fmap :: (a -> b) -> Stream a -> Stream b
    fmap f (x :> xs) = f x :> fmap f xs

instance Applicative Stream where
    -- pure :: a -> Stream a
    pure x = repeatS x 

    -- (<*>) :: Stream (a -> b) -> Stream a -> Stream b
    (<*>) (f :> fs) (x :> xs) = (f x :>) $ fs <*>  xs 

-- | The stream of fibonacci numbers. 0 1 1 2 3
fibS :: Stream Integer
fibS = fmap (fst) $ iterateS (\(x,y) -> (y, x + y) ) (0,1)

-- | The stream of prime numbers.
primeS :: Stream Integer
primeS = fmap (toInteger) $ filterS (\x -> isPrime x ) fullStream
  where
    fullStream   = fromS 3
    takeList i   = takeS (pred i) fullStream 
    isPrime x    = and [rem x y /= 0 | y <- [2..(x-1)]]

