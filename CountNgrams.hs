module CountNgrams where


import TextProcessors
import TextStatistics

import System.Environment



countngrams :: String -> Int -> IO ()
countngrams filename n = do
    contents <- readFile filename
    putStr $ printfreqlist (freqlist (ngrams n ((strip . (split " ")) contents)))

main = do
    (filename:n:_) <- getArgs


    
