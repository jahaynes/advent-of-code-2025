module Main ( main ) where

import           Data.Char       (isSpace)
import           Data.List.Split (splitOn)
import           Data.Set        (Set)
import qualified Data.Set as S

data Range =
    Range !Int !Int
        deriving Show

newtype Times =
    Times Int

candidates :: Times -> Range -> Set Int
candidates (Times times) (Range lo hi) = go S.empty lo

    where
    go !acc n
        | n > hi = acc
        | otherwise =
            let str = show n
                len = length str in
            case len `divMod` times of
                (fractionLen, 0) ->
                    let some    = take fractionLen str
                        bundled = read (concat (replicate times some)) in
                    if bundled > hi
                        then acc
                        else
                            if bundled < lo
                                then go acc (n + 1)
                                 else
                                    -- We should be able to skip to [some+1] 0000
                                    let newStart = show (read some + (1::Int))
                                        newEnd   = replicate (len - fractionLen) '0'
                                        newNum   = read (newStart ++ newEnd) in
                                    go (S.insert bundled acc) newNum
                -- Length not a multiple of times
                _ -> go acc (n + 1)

main :: IO ()
main = do
    ranges :: [Range] <- parse <$> readFile "./input_2"
    let times = Times <$> [2..7]
    print . sum
          $ mconcat ((\t -> mconcat (candidates t <$> ranges)) <$> times)

parse :: String -> [Range]
parse = map toRange 
      . splitOn ","
      . filter (not . isSpace)
    where
    toRange str =
        case splitOn "-" str of
            [lo, hi] -> Range (read lo) (read hi)
            _        -> error "Invalid input"
