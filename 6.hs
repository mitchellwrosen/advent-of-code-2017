{-# language BangPatterns #-}

import Data.List

import qualified Data.Set as Set

main :: IO ()
main = do
  let input = [4, 1, 15, 12, 0, 9, 9, 5, 5, 8, 7, 3, 14, 5, 12, 3]
  let xs = iterate redistribute input
  let n = unique xs
  let m = unique (drop n xs)
  print n
  print m

unique :: Ord a => [a] -> Int
unique = go 0 mempty
 where
  go !n seen (x:xs)
    | Set.member x seen = n
    | otherwise = go (n+1) (Set.insert x seen) xs

redistribute :: [Int] -> [Int]
redistribute xs =
  case elemIndex (maximum xs) xs of
    Just i ->
      case apply i zr (zeep xs) of
        Z xs y zs ->
          zoop (apply y (zr . zsucc) (zr (Z xs 0 zs)))

data Z a
  = Z [a] a [a]
  deriving Show

zeep :: [a] -> Z a
zeep (x:xs) = Z [] x xs

zoop :: Z a -> [a]
zoop (Z xs y zs) = reverse xs ++ [y] ++ zs

zr :: Z a -> Z a
zr (Z xs y zs) =
  case zs of
    [] ->
      case reverse (y:xs) of
        [x] -> Z [] x []
        x:xs -> Z [] x xs
    z:zs -> Z (y:xs) z zs

zsucc :: Z Int -> Z Int
zsucc (Z xs y zs) = Z xs (y+1) zs

apply :: Int -> (a -> a) -> a -> a
apply 0 _ = id
apply n f = apply (n-1) f . f
