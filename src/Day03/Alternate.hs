module Alternate ( main ) where

main :: IO ()
main = do
    input <- lines <$> readFile "input_3"
    let results :: [Int] = map read . map algo $ input
    print $ sum results

algo :: [Char] -> [Char]
algo _xs = go (length _xs) 12 [] _xs

    where
    go have want keep xs
        | have == want = reverse keep ++ xs
    go have want (k:ks) [] =
        go have want ks [k]
    go have want keep [z] =
        case keep of
            []     -> error "We couldn't backtrack"
            (k:ks) ->
                if k < z
                    then go (have-1) want    ks  [z]
                    else go (have-1) want (k:ks)  []
    go have want keep (a:b:xs) =
        if a >= b
            then go  have    want (a:keep) (b:xs)
            else
                case keep of
                    []     -> go (have-1) want []   (b:xs)
                    (k:ks) -> go (have-1) want ks (k:b:xs)
