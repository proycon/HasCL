module Test where

import Test.HUnit
import TextProcessors

teststring = ["to","be","or","not", "to", "be"]

ngramstest :: Test
ngramstest =
    TestCase $ assertEqual "Trigrams"
               [["to","be","or"],["be","or","not"],["or","not","to"],["not to be"]] 
               ngrams teststring
                           
                    

                
