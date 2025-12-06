module Combinators ( item
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