module TextStatistics where

import TextProcessors
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable
import Data.Maybe


-- absolute count
count :: (Eq a, Hashable a) => HashMap.HashMap a Int -> a -> Int
count freqlist token  = case value of Nothing -> 0
                                      Just x  -> x
                        where value = HashMap.lookup token freqlist

-- relative count (frequency)
freq :: (Eq a, Hashable a) => HashMap.HashMap a Int -> a -> Double
freq freqlist token = fromIntegral (count freqlist token) / fromIntegral (totalcount freqlist)

-- total number of tokens
--totalcount :: HashMap.HashMap a Int -> Int
--totalcount freqlist = sum (HashMap.elems freqlist)
totalcount :: (HashMap.HashMap a Int -> Int)
totalcount = sum . HashMap.elems

-- frequency list
freqlist :: (Eq a, Hashable a) => [a] -> HashMap.HashMap a Int 
freqlist []         = HashMap.empty
freqlist (token:xs) = HashMap.insert token ((count (freqlist xs) token) + 1) $ freqlist xs

-- ngram frequency list
ngramfreqlist :: Int -> [String] -> HashMap.HashMap [String] Int
ngramfreqlist n words = freqlist (ngrams n words)

-- print a frequency list
printfreqlist :: (Eq a, Hashable a, Show a) => HashMap.HashMap a Int -> String
printfreqlist freqlist = unlines [ show k ++ "\t" ++ show v | (k,v) <- HashMap.assocs freqlist ]
