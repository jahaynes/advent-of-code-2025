module String ( linesOf
              , pPositive
              , ws
              ) where

import Combinators
import Parser

import Control.Monad (void)
import Data.Char     (isDigit, isSpace)

linesOf :: Parser String a -> Parser String [a]
linesOf = sepBy (item (=='\n'))

ws :: Parser String ()
ws = void (pTakeWhile isSpace)

pPositive :: Parser String Int
pPositive = read <$> pTakeWhile1 isDigit