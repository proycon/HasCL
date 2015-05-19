module TextStatistics (count,addcount,addcount1,totalcount,freqlist,ngramfreqlist) where

import TextProcessors
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable
import Data.Maybe


-- absolute count
count :: (Eq a, Hashable a) => a -> HashMap.HashMap a Int -> Int
count ngram freqlist = case value of Nothing -> 0
                                     Just x  -> x
                       where value = HashMap.lookup ngram freqlist

-- relative frequency
freq :: (Eq a, Hashable a) => a -> HashMap.HashMap a Int -> Double
freq ngram freqlist = case value of Nothing -> 0.0
                                    Just x  -> (fromIntegral x) / (fromIntegral total)
                      where value = HashMap.lookup ngram freqlist
                            total = totalcount freqlist


totalcount :: HashMap.HashMap a Int -> Int
totalcount freqlist = sum (HashMap.elems freqlist)

freqlist :: (Eq a, Hashable a) => [a] -> HashMap.HashMap a Int 
freqlist []         = HashMap.empty
freqlist (token:xs) = HashMap.insert token ((count token $ freqlist xs) + 1) $ freqlist xs

ngramfreqlist :: Int -> [String] -> HashMap.HashMap [String] Int
ngramfreqlist n words = freqlist (ngrams n words)



