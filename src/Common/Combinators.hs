module Combinators ( item
                   , pTakeWhile
                   , pTakeWhile1
                   , sepBy
                   ) where

import Parser

import Control.Applicative (many)

item :: (s -> Bool) -> Parser [s] s
item p = Parser f
    where
    f     [] = Left "Out of input"
    f (x:xs) | p x = Right (xs, x)
             | otherwise = Left "Mismatch"

sepBy :: Parser s b -> Parser s a -> Parser s [a]
sepBy sep p = (:) <$> p <*> many (sep *> p)

pTakeWhile :: (a -> Bool) -> Parser [a] [a]
pTakeWhile p = Parser f
    where
    f s = let (ds, ss) = span p s in Right (ss, ds)


pTakeWhile1 :: (a -> Bool) -> Parser [a] [a]
pTakeWhile1 p = Parser f
    where
    f s = case span p s of
        ([],  _) -> Left "No matches"
        (ds, ss) -> Right (ss, ds)
