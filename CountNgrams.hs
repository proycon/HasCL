module CountNgrams where


import TextProcessors
import TextStatistics

import System.Environment



countngrams :: String -> IO ()
countngrams filename = do
    contents <- readFile filename
    putStr $ printfreqlist (freqlist contents)

main = do
    (filename:n:_) <- getArgs


    
