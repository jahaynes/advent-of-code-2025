module Main ( main ) where

import Data.Char (isDigit)
import Data.List (groupBy, sort)

data Range =
    Range !Int !Int
        deriving (Eq, Ord)

newtype Item =
    Item Int

main :: IO ()
main = do
    input <- readFile "input_5"
    let (ranges, items) = parse input
    print $ part1 ranges items
    print $ part2 ranges

part1 :: [Range] -> [Item] -> Int
part1 ranges = length . filter (\i -> any (included i) ranges)
    where
    included (Item i) (Range lo hi) =
        i >= lo && i <= hi

part2 :: [Range] -> Int
part2 = sum . map sz . walk . sort

    where
    walk :: [Range] -> [Range]
    walk = go Nothing
        where
        go mr [] =
            case mr of
                Just r  -> [r]
                Nothing -> []
        go Nothing (r:rs) = go (Just r) rs
        go (Just r1@(Range l1 h1)) (r2@(Range l2 h2):rs)
            | h1 >= l2  =      go (Just (Range l1 (max h1 h2))) rs
            | otherwise = r1 : go (Just            r2) rs

    sz :: Range -> Int
    sz (Range lo hi) = hi - lo + 1

parse :: String -> ([Range], [Item])
parse input =
    case groupBy (\a b -> null a == null b) $ lines input of
        [strRanges, [""], strItems] ->
            (map range strRanges, map (Item . read) strItems)
        _ -> error "Invalid input"
    where
    range s =
        case groupBy (\a b -> isDigit a == isDigit b) s of
            [strLo, "-", strHi] -> Range (read strLo) (read strHi)
            _ -> error "Invalid input"
