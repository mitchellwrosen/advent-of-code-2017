{-# language BangPatterns #-}

import Control.Monad.ST
import Data.IORef
import Data.Vector (Vector, (!?))

import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

main :: IO ()
main = do
  input <- Vector.fromList . map read . lines <$> readFile "input/5.txt"
  print (runST (part1 input))
  print (runST (part2 input))

part1 :: Vector Int -> ST s Int
part1 = go (+1)

part2 :: Vector Int -> ST s Int
part2 = go (\x -> if x >= 3 then x-1 else x+1)

go :: (Int -> Int) -> Vector Int -> ST s Int
go f xs = do
  v <- Vector.thaw xs
  let loop !c !i
        | i < 0 || i >= MVector.length v = pure c
        | otherwise = do
            x <- MVector.unsafeRead v i
            MVector.unsafeWrite v i $! f x
            loop (c+1) (i+x)
  loop 0 0
