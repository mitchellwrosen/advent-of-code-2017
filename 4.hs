import Data.List (nub, sort)

main :: IO ()
main = do
  input <- readFile "input/4.txt"
  print (part1 input)
  print (part2 input)

part1 :: String -> Int
part1 = length . filter (unique . words) . lines

part2 :: String -> Int
part2 = length . filter (unique . map sort . words) . lines

unique :: [String] -> Bool
unique xs = length xs == length (nub xs)
