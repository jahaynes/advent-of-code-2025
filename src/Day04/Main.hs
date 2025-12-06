module Main ( main ) where

import           Data.Functor         ((<&>))
import           Data.Map             (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe           (mapMaybe)

newtype Row =
    Row Int deriving (Eq, Ord, Show)

newtype Col =
    Col Int deriving (Eq, Ord, Show)

rows :: [Row]
rows = Row <$> [0..]

cols :: [Col]
cols = Col <$> [0..]

dirs :: (Row, Col) -> [(Row, Col)]
dirs (Row r, Col c) = [up, down, left, right, ul, ur, dl, dr]
    where
    up    = (Row $ r - 1, Col c)
    down  = (Row $ r + 1, Col c)
    left  = (Row r, Col $ c - 1)
    right = (Row r, Col $ c + 1)
    ul    = (Row $ r - 1, Col $ c - 1)
    ur    = (Row $ r - 1, Col $ c + 1)
    dl    = (Row $ r + 1, Col $ c - 1)
    dr    = (Row $ r + 1, Col $ c + 1)

main :: IO ()
main = do
    grid0 <- asGrid <$> readFile "input_4"
    print $ part1 grid0
    print $ part2 grid0

part1 :: Map (Row, Col) Char -> Int
part1 = length . filter (\(_, c, ns) -> c == '@' && length ns < 4)
               . neighbours

part2 :: Map (Row, Col) Char -> Int
part2 = go 0
    where
    go acc grid =
        let toRemove = map (\(k, _, _) -> k)
                     . filter (\(_, c, ns) -> c == '@' && length ns < 4)
                     . neighbours
                     $ grid
            removeCount = length toRemove in
        if removeCount == 0
            then acc
            else
                let grid' = foldr (M.update (\'@' -> Just '.')) grid toRemove
                    acc'  = acc + removeCount
                in go acc' grid'

neighbours :: Map (Row, Col) Char -> [((Row, Col), Char, [((Row, Col), Char)])]
neighbours grid = lkup (M.toList grid)
    where
    lkup keyVals = keyVals <&> \(k, v) ->
        let ns = filter (\(_, c) -> c == '@')
               . mapMaybe (\d -> (d,) <$> M.lookup d grid)
               $ dirs k in
        (k, v, ns)

asGrid :: String -> Map (Row, Col) Char
asGrid = M.fromList . withCoords

    where
    withCoords :: String -> [((Row, Col), Char)]
    withCoords = concatMap f . zip rows . lines
        where
        f (row, xs) = zipWith (\col x -> ((row, col), x)) cols xs
