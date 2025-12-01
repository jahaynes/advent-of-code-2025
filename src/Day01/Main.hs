module Main ( main ) where

newtype Rot = Rot Int
newtype Pos = Pos Int
newtype Clicks = Clicks Int
newtype Zeros = Zeros Int

main :: IO ()
main = do

    input <- map parse . lines <$> readFile "./input_1"

    let (_, Zeros part1, Clicks part2) = foldl step (Pos 50, Zeros 0, Clicks 0) input

    print part1
    print part2

    where
    parse ('L':n) = Rot (- read n)
    parse ('R':n) = Rot (  read n)
    parse       _ = error "Bad input"

step :: (Pos, Zeros, Clicks) -> Rot -> (Pos, Zeros, Clicks)
step (pos, Zeros z, Clicks ca) rot = do

    let (Pos pos', Clicks cb) = pos ⟳ rot

    let z' = if pos' `mod` 100 ==  0
                 then z + 1
                 else z

    (Pos pos', Zeros z', Clicks $ ca + cb)

(⟳) :: Pos -> Rot -> (Pos, Clicks)
Pos p ⟳ Rot r =

    let p' = p + r
        da = p  `div` 100
        db = p' `div` 100 in

    if | r  > 0 -> let clicks = abs (db - da)
                   in (Pos p', Clicks clicks)

       | r  < 0 -> let clicks = abs (db - da)
                              + if p  `mod` 100 == 0 then -1 else 0
                              + if p' `mod` 100 == 0 then  1 else 0
                   in (Pos p', Clicks clicks)

       | otherwise -> (Pos p, Clicks 0)
