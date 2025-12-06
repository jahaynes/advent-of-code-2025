module Main ( main ) where

import Combinators
import Parser
import String

import Control.Applicative
import Control.Monad
import Data.List           (transpose)

data Item = Number !Int
          | Op !Char

main :: IO ()
main = do

    input <- parse <$> readFile "./sample_6"

    print $ part1 input

part1 :: [[Item]] -> Int
part1 = sum . map row . transpose
    where
    row :: [Item] -> Int
    row xs =
        let nums   = map (\(Number n) -> n) . init $ xs
            op     = last xs
            (o, z) = case op of
                         Op '*' -> ((*), 1)
                         Op '+' -> ((+), 0) in
        foldl o z nums

parse :: String -> [[Item]]
parse s =
    case runParser (linesOf itemLine <* ws) s of
        Right ([], xss) -> xss
        Right (lo,   _) -> error $ "Leftover: " ++ show lo
        Left e -> error e

itemLine :: Parser String [Item]
itemLine = ws *> sepBy gap pItem <* pTakeWhile (== ' ')
    where
    gap :: Parser String ()
    gap = void (pTakeWhile1 (== ' '))

    pItem :: Parser String Item
    pItem  =  Number <$> pPositive
          <|> Op     <$> pOp
        where
        pOp = item (\c -> c == '+' || c == '*')
