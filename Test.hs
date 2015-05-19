module Test where

import Test.HUnit
import TextProcessors
import TextStatistics

testtokens = ["to","be","or","not", "to", "be"]

ngramstest =
    TestCase $ assertEqual "Trigrams"
               [["to","be","or"],["be","or","not"],["or","not","to"],["not","to","be"]] 
               (ngrams 3 testtokens)


freqlisttest1 = 
    TestCase $ assertEqual "Frequency List, count of word 'be'"
               2
               (count (freqlist testtokens) "be")
               
freqlisttest2 = 
    TestCase $ assertEqual "Frequency List, count of word 'or'"
               1
               (count (freqlist testtokens) "or")

freqlisttest3 = 
    TestCase $ assertEqual "Frequency List, count of absent word 'blah'"
               0
               (count (freqlist testtokens) "blah")

freqlisttest4 = 
    TestCase $ assertEqual "Frequency List, total count"
               (length testtokens)
               (totalcount (freqlist testtokens))

main = runTestTT $ TestList [ngramstest, freqlisttest1, freqlisttest2, freqlisttest3, freqlisttest4]
                           
                    

                
