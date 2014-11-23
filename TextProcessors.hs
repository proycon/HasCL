module TextProcessors (ngrams) where

ngrams :: Int -> [a] -> [[a]]
ngrams n v@(x:xs)
    | length v < n = []
    | otherwise    = take n v : (ngrams n xs)


    







