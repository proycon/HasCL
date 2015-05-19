module Test where

import Test.HUnit
import TextProcessors
import TextStatistics

teststring = ["to","be","or","not", "to", "be"]

ngramstest =
    TestCase $ assertEqual "Trigrams"
               [["to","be","or"],["be","or","not"],["or","not","to"],["not","to","be"]] 
               (ngrams 3 teststring)

main = runTestTT ngramstest
                           
                    

                
