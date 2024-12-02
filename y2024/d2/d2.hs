import Data.List

isSafe :: [Int] -> Bool
isSafe r = any safe [r, negate <$> r]
  where
    safe l = all (\x -> x `elem` [1, 2, 3]) $ elemDiff l
    elemDiff l = zipWith (-) l $ drop 1 l

removeLevel :: [a] -> [[a]]
removeLevel [] = []
removeLevel (x : xs) = xs : map (x :) (removeLevel xs)

removedSafe :: [Int] -> Bool
removedSafe l = any isSafe $ removeLevel l

main :: IO ()
main = do
  reports :: [[Int]] <- map (map read . words) . lines <$> readFile "day2.txt"
  print $ length $ filter isSafe reports
  print $ length $ filter removedSafe reports
