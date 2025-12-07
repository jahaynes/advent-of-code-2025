module Main ( main ) where

import           Data.List            (groupBy)
import qualified Data.Map.Strict as M
import           Data.Map             (Map)

newtype Row =
    Row Int deriving (Eq, Ord, Show)

newtype Col =
    Col Int deriving (Eq, Ord, Show)

newtype Manifold =
    Manifold (Map (Row, Col) Char)
        deriving Eq

instance Show Manifold where
    show (Manifold m) = unlines
                      . map (map snd)
                      . groupBy (\((r1,_),_) ((r2,_),_) -> r1 == r2)
                      $ M.toAscList m

main :: IO ()
main = do
    manifold0 <- parse <$> readFile "./sample_7"
    print $ step manifold0
    
    let final = whileChanged step manifold0

    print $ countSplits final


whileChanged :: Eq a => (a -> a) -> a -> a
whileChanged f x
    | x == y = x
    | otherwise = whileChanged f y
        where y = f x 

countSplits :: Manifold -> Int
countSplits (Manifold man) = sum
                           . map snd
                           . M.toList 
                           . M.mapWithKey count'
                           $ man
    where
    count' :: (Row, Col) -> Char -> Int
    count' (Row r, Col c) x
        | x == '^' && up == Just '|' = 1
        | otherwise                  = 0
            where
            up = M.lookup (Row (r-1), Col c) man

step :: Manifold -> Manifold
step (Manifold man) = Manifold (M.mapWithKey step' man)
    where
    step'              _ '|' = '|'
    step'              _ '^' = '^'
    step'              _ 'S' = 'S'
    step' (Row r, Col c) '.' = do
        let fromAbove =
                case up of
                    Just '|' -> True
                    Just 'S' -> True
                    _        -> False
            fromLeft =
                (left, upLeft)   == (Just '^', Just '|')
            fromRight =
                (right, upRight) == (Just '^', Just '|')
        if fromAbove || fromLeft || fromRight
            then '|'
            else '.'
        where
        up =
            M.lookup (Row (r-1), Col  c   ) man
        right =
            M.lookup (Row  r   , Col (c+1)) man
        upRight =
            M.lookup (Row (r-1), Col (c+1)) man
        left =
            M.lookup (Row  r   , Col (c-1)) man
        upLeft =
            M.lookup (Row (r-1), Col (c-1)) man
    step' _ _ = error "bad"

parse :: String -> Manifold
parse = Manifold . M.fromList . withCoords

    where
    withCoords :: String -> [((Row, Col), Char)]
    withCoords = concatMap f . zip rows . lines
        where
        f (row, xs) = zipWith (\col x -> ((row, col), x)) cols xs

    rows :: [Row]
    rows = Row <$> [0..]

    cols :: [Col]
    cols = Col <$> [0..]