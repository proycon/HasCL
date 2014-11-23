module TextStatistics (getcount,count,count1,makefreqlist,ngramfreqlist) where
import TextProcessors
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable
import Data.Maybe


getcount :: (Eq a, Hashable a) => a -> HashMap.HashMap a Int -> Int
getcount ngram freqlist = if value == Nothing then 0 else fromJust value
                     where value = HashMap.lookup ngram freqlist

count :: (Eq a, Hashable a) => a -> Int -> HashMap.HashMap a Int -> HashMap.HashMap a Int
count ngram c freqlist= HashMap.insert ngram ((getcount ngram freqlist) + c) freqlist

count1 :: (Eq a, Hashable a) => a -> HashMap.HashMap a Int -> HashMap.HashMap a Int
count1 ngram freqlist = count ngram 1 freqlist

makefreqlist :: (Eq a, Hashable a) => [a] -> HashMap.HashMap a Int 
makefreqlist []         = HashMap.empty
makefreqlist (ngram:xs) = count1 ngram (makefreqlist xs) 

ngramfreqlist :: Int -> [String] -> HashMap.HashMap [String] Int
ngramfreqlist n words = makefreqlist (ngrams n words)



