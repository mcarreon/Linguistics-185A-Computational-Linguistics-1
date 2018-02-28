module ProbBigrams where

import qualified Corpus01
import qualified Corpus02a
import qualified Corpus02b
import qualified Data.Map as Map

-----------------------------------------------------------------
-----------------------------------------------------------------
-- Some general helper functions

-- Adds one to the value associated with the given key.
addOne :: (Ord a) => a -> Map.Map a Int -> Map.Map a Int
addOne x m =
    if Map.member x m then
        Map.adjust (\n -> n+1) x m
    else
        Map.insert x 1 m

-- Divides one integer by another, in a way that hides some annoying distractions.
divisionHelper :: Int -> Int -> Double
divisionHelper x y = if y == 0 then 0 else (fromIntegral x / fromIntegral y)

-- trainModel produces a trained model on the basis of some training data.
-- The first argument (addBigram) is a function that can update such a model on the basis of 
-- some particular bigram observed in the training corpus.
-- The second argument is the training corpus (a list of sentences).
-- The third argument is an existing model that we wish to ``update'' with training data.
trainModel :: ((String,String) -> a -> a) -> [String] -> a -> a
trainModel addBigram [] m = m
trainModel addBigram (s:ss) m = updateForSent addBigram (prep s) (trainModel addBigram ss m)

-- updateForSent is like trainModel, but for a single sentence represented as a 
-- list of words (with start and end markers already present).
updateForSent :: ((String,String) -> a -> a) -> [String] -> a -> a
updateForSent addBigram [] m = m
updateForSent addBigram (w:[]) m = m
updateForSent addBigram (w:x:rest) m = addBigram (w,x) (updateForSent addBigram (x:rest) m)

-- probOfSent calculates the probability of a full sentence.
-- The first argument is a function that returns the probability of a given bigram.
-- The second argument is a sentence represented as a list of words (with start and 
-- end markers already present).
probOfSent :: ((String,String) -> Double) -> [String] -> Double
probOfSent f [] = 1.0
probOfSent f (w:[]) = 1.0
probOfSent f (w:x:rest) = f (w,x) * probOfSent f (x:rest)

-- probOfSents is like probOfSents, but for a list of sentences.
probOfSents :: ((String,String) -> Double) -> [String] -> Double
probOfSents f sents = product (map (\sent -> probOfSent f (prep sent)) sents)

-- Converts a space-separated sentence into a list of words, plus 
-- the start and end markers.
prep :: String -> [String]
prep sent = ["<s>"] ++ words sent ++ ["</s>"]

-----------------------------------------------------------------
-----------------------------------------------------------------
-- The ``simple'' bigram model

type SimpleModel = (Map.Map String Int, Map.Map (String,String) Int)

freshSimpleModel :: SimpleModel
freshSimpleModel = (Map.empty, Map.empty)

addBigramSimple :: (String,String) -> SimpleModel -> SimpleModel
addBigramSimple (x,y) (unigrams, bigrams) =
    (addOne x unigrams, addOne (x,y) bigrams)

bigramProbSimple :: SimpleModel -> (String,String) -> Double
bigramProbSimple (unigrams, bigrams) (x,y) =
    let num = Map.findWithDefault 0 (x,y) bigrams in
    let denom = Map.findWithDefault 0 x unigrams in
    divisionHelper num denom

trainTestSimple :: [String] -> [String] -> Double
trainTestSimple training test =
    let model = trainModel addBigramSimple training freshSimpleModel in
    probOfSents (bigramProbSimple model) test

-----------------------------------------------------------------
-----------------------------------------------------------------
-- The length-based bigram model

-- You can change the definitions of these types if you wish.
data LengthCat = Long | Short deriving (Eq,Show,Ord)
type LengthModel = (Map.Map LengthCat Int, Map.Map (LengthCat,String) Int)

freshLengthModel :: LengthModel
freshLengthModel = (Map.empty, Map.empty)

addBigramByLength :: (String,String) -> LengthModel -> LengthModel
addBigramByLength (x,y) (unigrams, bigrams) = if length (x) > 4 then (addOne Long unigrams, addOne (Long,y) bigrams) else (addOne Short unigrams, addOne (Short,y) bigrams)
    

bigramProbByLength :: LengthModel -> (String,String) -> Double
bigramProbByLength (unigrams, bigrams) (x,y) = if length (x) > 4 
    then (
        let num = Map.findWithDefault 0 (Long,y) bigrams in
        let denom = Map.findWithDefault 0 Long unigrams in
        divisionHelper num denom
    )
    else (
        let num = Map.findWithDefault 0 (Short,y) bigrams in
        let denom = Map.findWithDefault 0 Short unigrams in
        divisionHelper num denom
    )

trainTestLength :: [String] -> [String] -> Double
trainTestLength training test =
    let model = trainModel addBigramByLength training freshLengthModel in
    probOfSents (bigramProbByLength model) test

-----------------------------------------------------------------
-----------------------------------------------------------------
-- The vowel-based bigram model

-- You can change the definitions of these types if you wish.
data VowelCat = Vowel | Cons deriving (Eq,Show,Ord)
type VowelModel = (Map.Map VowelCat Int, Map.Map (VowelCat,String) Int)

freshVowelModel :: VowelModel
freshVowelModel = (Map.empty, Map.empty)

isVowel :: [Char] -> Bool
isVowel (x:[]) = case x of 
    'a' -> True
    --'A' -> True
    'e' -> True
    --'E' -> True
    'i' -> True
    --'I' -> True
    'o' -> True
    --'O' -> True
    'u' -> True
    --'U' -> True
    '-' -> False
    _ -> False 
isVowel (x:xs) = case x of 
    'a' -> True
    --'A' -> True
    'e' -> True
    --'E' -> True
    'i' -> True
    --'I' -> True
    'o' -> True
    --'O' -> True
    'u' -> True
    --'U' -> True
    '-' -> False
    _ -> False 

isVowel [] = False


addBigramByVowel :: (String,String) -> VowelModel -> VowelModel
addBigramByVowel (x,y) (unigrams, bigrams) = if isVowel(x) == True then (addOne Vowel unigrams, addOne (Vowel,y) bigrams) else (addOne Cons unigrams, addOne (Cons,y) bigrams)

bigramProbByVowel :: VowelModel -> (String,String) -> Double
bigramProbByVowel (unigrams, bigrams) (x,y) = if isVowel(x) == True 
    then (
        let num = Map.findWithDefault 0 (Vowel,y) bigrams in
        let denom = Map.findWithDefault 0 Vowel unigrams in
        divisionHelper num denom
    )
    else (
        let num = Map.findWithDefault 0 (Cons,y) bigrams in
        let denom = Map.findWithDefault 0 Cons unigrams in
        divisionHelper num denom
    )

trainTestVowel :: [String] -> [String] -> Double
trainTestVowel training test =
    let model = trainModel addBigramByVowel training freshVowelModel in
    probOfSents (bigramProbByVowel model) test

-----------------------------------------------------------------
-----------------------------------------------------------------
-- The interpolated model

bigramProbByInterpolated :: Double -> LengthModel -> VowelModel -> (String,String) -> Double
bigramProbByInterpolated n lengthModel vowelModel (x,y) = ((bigramProbByLength lengthModel (x,y)) * n) + ((bigramProbByVowel vowelModel (x,y)) * (1 - n))

trainTestInterpolated :: Double -> [String] -> [String] -> Double
trainTestInterpolated x training test = let lengthModel = trainModel addBigramByLength training freshLengthModel in 
    let vowelModel = trainModel addBigramByVowel training freshVowelModel in 
      probOfSents (bigramProbByInterpolated x lengthModel vowelModel) test

   
--trainTestInterpolated x training test = let n = (let model = trainModel addBigramByLength training freshLengthModel in (probOfSents (bigramProbByLength model) test)) in 
--    let m = (let model = trainModel addBigramByVowel training freshVowelModel in (probOfSents (bigramProbByVowel model) test)) in
--   ()

      
    
    
    
