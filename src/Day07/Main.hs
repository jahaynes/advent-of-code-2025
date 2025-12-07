module Main ( main ) where

import Combinators
import Parser
import State
import String

import           Control.Applicative
import           Control.Monad
import qualified Data.Map.Strict as M
import           Data.Map (Map)

newtype Row =
    Row Int deriving (Eq, Ord, Show)

newtype Col =
    Col Int deriving (Eq, Ord, Show)

newtype Manifold =
    Manifold (Map (Row, Col) Char)
        deriving Show

main :: IO ()
main = do

    manifold0 <- parse <$> readFile "./sample_7"

    print $ step manifold0
 
    pure ()

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
        if or [fromAbove, fromLeft, fromRight]
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