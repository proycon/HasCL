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

-- relative frequency
freq :: (Eq a, Hashable a) => HashMap.HashMap a Int -> a -> Double
freq freqlist token = case value of Nothing -> 0.0
                                    Just x  -> (fromIntegral x) / (fromIntegral total)
                      where value = HashMap.lookup token freqlist
                            total = totalcount freqlist


totalcount :: HashMap.HashMap a Int -> Int
totalcount freqlist = sum (HashMap.elems freqlist)

freqlist :: (Eq a, Hashable a) => [a] -> HashMap.HashMap a Int 
freqlist []         = HashMap.empty
freqlist (token:xs) = HashMap.insert token ((count (freqlist xs) token) + 1) $ freqlist xs

ngramfreqlist :: Int -> [String] -> HashMap.HashMap [String] Int
ngramfreqlist n words = freqlist (ngrams n words)




