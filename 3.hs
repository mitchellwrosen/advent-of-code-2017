{-# language DeriveFunctor #-}

import Control.Monad
import Data.Function
import Data.Maybe
import Debug.Trace

(<!) :: Show a => a -> String -> a
(<!) x s = trace (s ++ show x) x

main :: IO ()
main = do
  let input = 325489
  print (part1 input)
  part2 input

part1 :: Int -> Int
part1 n =
  let
    x = floor (sqrt (fromIntegral n))
    y = (x * x)
  in
    if n == y
      then (x-1)
      else
          min (abs (n - y - div x 2 - 1))
              (abs (n - y - x - div (x+1) 2 - 1))
            + div (x+1) 2

-- Couldn't figure out a closed form solution... so I brute-forced the hell out
-- of it with Debug.Trace-style logging.
part2 :: Int -> IO ()
part2 input = go 1 initial
 where
  go n z =
    when (fromJust (zzval z) <= input)
      (go (n+2) (spiral n z))

data S a
  = S a (S a)
  deriving Functor

srepeat :: a -> S a
srepeat x = S x (srepeat x)

data Z a
  = Z (S a) a (S a)

zpure :: a -> Z a
zpure x = Z (srepeat x) x (srepeat x)

zval :: Z a -> a
zval (Z _ x _) = x

zset :: a -> Z a -> Z a
zset y (Z xs _ zs) = Z xs y zs

zL :: Z a -> Z a
zL (Z (S x xs) y zs) = Z xs x (S y zs)

zR :: Z a -> Z a
zR (Z xs y (S z zs)) = Z (S y xs) z zs

type ZZ a
  = Z (Z a)

zzval :: ZZ a -> a
zzval = zval . zval

zzset :: a -> ZZ a -> ZZ a
zzset y (Z as b cs) = Z as (zset y b) cs

zzU :: ZZ a -> ZZ a
zzU = zL

zzD :: ZZ a -> ZZ a
zzD = zR

zzL :: ZZ a -> ZZ a
zzL (Z xs y zs) = Z (fmap zL xs) (zL y) (fmap zL zs)

zzR :: ZZ a -> ZZ a
zzR (Z xs y zs) = Z (fmap zR xs) (zR y) (fmap zR zs)

initial :: ZZ (Maybe Int)
initial = Z a b a
 where
  a = srepeat (zpure Nothing)
  b = Z c (Just 1) c
  c = srepeat Nothing

neighbors :: ZZ (Maybe Int) -> Int
neighbors z =
  sum
    (catMaybes
      [ zzval (zzR z)
      , zzval (zzU (zzR z))
      , zzval (zzU z)
      , zzval (zzL (zzU z))
      , zzval (zzL z)
      , zzval (zzD (zzL z))
      , zzval (zzD z)
      , zzval (zzR (zzD z))
      ])

ploop :: ZZ (Maybe Int) -> ZZ (Maybe Int)
ploop z = zzset (Just (neighbors z <! "")) z

spiral :: Int -> ZZ (Maybe Int) -> ZZ (Maybe Int)
spiral n z = z
  & zzR
  & ploop
  & apply n (ploop . zzU)
  & apply (n+1) (ploop . zzL)
  & apply (n+1) (ploop . zzD)
  & apply (n+1) (ploop . zzR)

apply :: Int -> (a -> a) -> a -> a
apply 0 _ = id
apply n f = apply (n-1) f . f
