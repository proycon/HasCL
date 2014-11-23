module TextStatistics (count,addcount,addcount1,totalcount,freqlist,ngramfreqlist) where
import TextProcessors
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable
import Data.Maybe


count :: (Eq a, Hashable a) => a -> HashMap.HashMap a Int -> Int
count ngram freqlist = case value of Nothing -> 0
                                     Just x  -> x
                       where value = HashMap.lookup ngram freqlist

freq :: (Eq a, Hashable a) => a -> HashMap.HashMap a Int -> Double
freq ngram freqlist = case value of Nothing -> 0.0
                                    Just x  -> (fromIntegral x) / (fromIntegral x)
                      where value = HashMap.lookup ngram freqlist

totalcount :: HashMap.HashMap a Int -> Int
totalcount freqlist = sum (HashMap.elems freqlist)

addcount :: (Eq a, Hashable a) => a -> Int -> HashMap.HashMap a Int -> HashMap.HashMap a Int
addcount ngram c freqlist= HashMap.insert ngram ((count ngram freqlist) + c) freqlist

addcount1 :: (Eq a, Hashable a) => a -> HashMap.HashMap a Int -> HashMap.HashMap a Int
addcount1 ngram freqlist = addcount ngram 1 freqlist


freqlist :: (Eq a, Hashable a) => [a] -> HashMap.HashMap a Int 
freqlist []         = HashMap.empty
freqlist (ngram:xs) = addcount1 ngram (freqlist xs) 

ngramfreqlist :: Int -> [String] -> HashMap.HashMap [String] Int
ngramfreqlist n words = freqlist (ngrams n words)



