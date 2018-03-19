module ProbCFG where

---------------------------------------------------------------

import qualified Data.Map as Map

-- A useful helper for debugging with Map. Feel free to ignore 
-- the implementation of this.
printMap :: (Show k, Show v) => Map.Map k v -> IO ()
printMap = putStr . unlines . map show . Map.toList

---------------------------------------------------------------

data Cat = S | NP | VP | N | D | V | PP | P | Adv deriving (Show,Eq,Ord)

data StrucDesc = Leaf Cat String | Binary Cat StrucDesc StrucDesc
                 deriving Show

type ProbCFG = ([(Cat,Double)],
                [((Cat,String),Double)],        -- terminal rules
                [((Cat,(Cat,Cat)),Double)],     -- nonterminal rules
                [Cat])

-- A minor variant of the grammar on page 384 of 
-- Manning and Schutze's "Foundations of Statistical NLP"
pcfg1 :: ProbCFG
pcfg1 = (   [(S,1.0)] ,
            [((P,"with"), 1.0), 
             ((V,"saw"), 1.0), 
             ((NP,"dogs"), 0.1), ((NP,"telescopes"), 0.18), ((NP,"saw"), 0.04), ((NP,"cats"), 0.18), ((NP,"hamsters"), 0.1)] ,
            [((S,(NP,VP)), 1.0),
             ((PP,(P,NP)), 1.0),
             ((VP,(V,NP)), 0.7), ((VP,(VP,PP)), 0.3),
             ((NP,(NP,PP)), 0.4)] ,
            [S,NP,VP,PP,P,V]
        )

-- Like above but reversed probabilities on the rules for expanding VP
pcfg2 :: ProbCFG
pcfg2 = (   [(S,1.0)] ,
            [((P,"with"), 1.0), 
             ((V,"saw"), 1.0), 
             ((NP,"dogs"), 0.1), ((NP,"telescopes"), 0.18), ((NP,"saw"), 0.04), ((NP,"cats"), 0.18), ((NP,"hamsters"), 0.1)] ,
            [((S,(NP,VP)), 1.0),
             ((PP,(P,NP)), 1.0),
             ((VP,(V,NP)), 0.3), ((VP,(VP,PP)), 0.7),
             ((NP,(NP,PP)), 0.4)] ,
            [S,NP,VP,PP,P,V]
        )

--------------------------------------------------
-- Utility functions for getting information from grammars.

probLookup :: (Eq a) => [(a,Double)] -> a -> Double
probLookup []           key = 0.0
probLookup ((x,y):rest) key = if key == x then y else probLookup rest key

allCats :: ProbCFG -> [Cat]
allCats (starting,ending,transitions,cats) = cats

startProb :: ProbCFG -> Cat -> Double
startProb (starting,ending,transitions,cats) = probLookup starting

endProb :: ProbCFG -> Cat -> String -> Double
endProb (starting,ending,transitions,cats) c s = probLookup ending (c,s)

trProb :: ProbCFG -> Cat -> (Cat,Cat) -> Double
trProb (starting,ending,transitions,cats) c (c1,c2) = probLookup transitions (c,(c1,c2))

-------------------------------------------------------------
-- Simple recursive definition of inside probabilities

naiveInside :: ProbCFG -> [String] -> Cat -> Double
naiveInside cfg [] cat = undefined
naiveInside cfg (w:[]) cat = 
    endProb cfg cat w
naiveInside cfg w cat = sum [trProb cfg cat (x,y) *  naiveInside cfg (take n (w)) x * naiveInside cfg (drop n (w)) y | x <- allCats cfg, y <- allCats cfg, n <- [1 .. length w-1]]
    

--naiveBackward g []     st = endProb g st
--naiveBackward g (w:ws) st =
--    sum (map (\next -> trProb g st next * emProb g (st,next) w * naiveBackward g ws next) (allStates g))

-------------------------------------------------------------

-- A name for the specific Map type that we will use to represent 
-- a table of inside probabilities.
type InsideTable = Map.Map ([String],Cat) Double

fastInside :: ProbCFG -> [String] -> Cat -> Double
fastInside g sent c =
    Map.findWithDefault 0 (sent,c) (buildInsideTable g sent)

buildInsideTable :: ProbCFG -> [String] -> InsideTable
buildInsideTable g sent =
    fillCellsInside g Map.empty (cellsToFill g sent)

cellsToFill :: ProbCFG -> [String] -> [([String],Cat)]
cellsToFill g sent = [(chunk,cat) | chunk <- chunks sent, cat <- allCats g]

chunks :: [String] -> [[String]]
chunks [] = []
chunks xs = [take n xs | n <- [1 .. length xs]] ++ chunks (drop 1 xs)

-- Returns all the suffixes of a list, in longest-to-shortest order.
--suffixes :: [a] -> [[a]]
--suffixes xs = [drop n xs | n <- reverse [0 .. length xs]]

fillCellsInside :: ProbCFG -> InsideTable -> [([String],Cat)] -> InsideTable
fillCellsInside g tbl [] = tbl
--fillCellsInside g tbl ((sentenceChunk, cat):rest) = 
--    let result = 
--            case sentenceChunk of
--                [] -> endProb g cat
--                (w:ws) -> let probInside = \gram -> \ys -> \cat -> Map.findWithDefault 0 (ys, cat) gram tbl in sum [trProb g cat (x,y) * probInside (drop n (w)) g y * probInside (take n (w)) g x | x <- allCats g, y <- allCats g, n <- [1 .. length w-1]]                 
--    in
--        let updatedTbl =
--             if (result > 0) then
--                Map.insert (sentenceChunk, cat) result tbl
--            else
--                tbl
--        in 
--    fillCellsInside g updatedTbl rest

--fillCellsBackward :: ProbFSA -> BackwardTable  -> [([String],State)] -> BackwardTable
--fillCellsBackward g tbl [] = tbl
--fillCellsBackward g tbl ((sentenceChunk,st):rest) =
--    -- First calculate the probability for (sentenceChunk,st)
--    let result =
--            case sentenceChunk of
--            [] -> endProb g st
--            (w:ws) -> let probBackward = \ys -> \st -> Map.findWithDefault 0 (ys,st) tbl in
--                      sum (map (\next -> trProb g st next * emProb g (st,next) w * probBackward ws next) (allStates g))
--    in
--    -- Now add the calculated probability to the table, if nonzero
--    let updatedTbl =
--            if (result > 0) then
--                Map.insert (sentenceChunk,st) result tbl
--            else
--                tbl
--    in
--    -- Continue on with the rest of the cells to be filled
--    fillCellsBackward g updatedTbl rest

-------------------------------------------------------------

-- A name for the specific Map type that we will use to represent 
-- a table of viterbi probabilities and backpointers.
type ViterbiTable = (Map.Map ([String],Cat) Double, Map.Map ([String],Cat) (Cat,Cat,Int))

buildViterbiTable :: ProbCFG -> [String] -> ViterbiTable
buildViterbiTable = undefined

extractStrucDesc :: ViterbiTable -> ([String],Cat) -> StrucDesc
extractStrucDesc = undefined

