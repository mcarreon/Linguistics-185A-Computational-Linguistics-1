module LoopDetection where

import Bigrams_Stub

type Position = ([String], String)

canLoop :: [GrammarRule] -> String -> Bool 
canLoop g s = canLoop' g ([],s)

canLoop' :: [GrammarRule] -> String -> Bool 
canLoop' g (history, w) = 
--nextWords is all things we might move to next after w
let nextWords = successors g w in 
    if any (\nextWord -> elem nextWord history) nextWords then True 
		--if any of the nextwords are already in the history, then true
else ()
--otherwise, create position having moved to each next word 
--and see if we can reach a loop from any of those 
--the call to any will return false whenever some new positions is empty
--ie whenever nextWords is empty

let newPositions = ........ (history ++ [w], nextWord)) nextWords in 
any (canLoop' g) newPositions )