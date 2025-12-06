module Main ( main ) where

import Data.Char (intToDigit)

main :: IO ()
main = do

    input <- lines <$> readFile "input_3"

    let part1 = sum . map (processLine  2 . (map (\a -> read [a]))) $ input
    let part2 = sum . map (processLine 12 . (map (\a -> read [a]))) $ input

    print part1
    print part2

processLine :: Int -> [Int] -> Int
processLine total = go []

    where
    go acc [] = read . map intToDigit $ acc
    go acc (x:xs) =
        let safeAt = total - length (x:xs)
            (safe, consider) = splitAt safeAt acc
            consider' = bestPrefix consider in
        go (take total (safe ++ consider')) xs
        where
        bestPrefix [] = [x]
        bestPrefix (a:cc)
            | x > a = [x]
            | otherwise = a : bestPrefix cc
