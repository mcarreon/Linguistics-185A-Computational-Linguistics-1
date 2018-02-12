module Section03 where

data GrammarRule = Step String String | End String deriving Show
data StrucDesc = Last String | NonLast String StrucDesc deriving Show
data Numb = Z | S Numb deriving Show

applyToAll :: (a -> b) -> [a] -> [b]
applyToAll f [] = []
applyToAll f (x:xs) = (f x) : (applyToAll f xs)

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
--head and tails of the list
suffixes (x:xs) = (x:xs) : (suffixes xs)
--have to have itself, and then the end of the list

skipEverySecond :: [a] -> [a] 
--if it is empty/base case
skipEverySecond [] = []
--second base case, for one element
skipEverySecond (x:[]) = [x]
-- recursion step
skipEverySecond (x:(y:ys)) = x : (skipEverySecond ys)

inverseMap :: [(a -> b)] -> a -> [b] 
--base case empty arg list
inverseMap [] n = []
--the first one will be the function f applied to the argument, then inverseMap the rest of the functions 
inverseMap (f:fs) n = f n : (inverseMap fs n)

beforeAndAfter :: (a -> b) -> [a] -> [(a,b)]
--base case taking a function and list, empty list 
beforeAndAfter f [] = [] 
--recursion step, the first will be a tuple argument, then function applied to second argument
beforeAndAfter f (x:xs) = (x, f x) : (beforeAndAfter f xs)

oneStep :: Int -> [Int]
oneStep x = [2*x + 1, x*x + x]

reachable :: Numb -> Int -> [Int]
--base case is Z 
--returns list containing the argument, 
reachable Z x = [x]
--recursive step
--reachable takes the Numb and applies it to onestep, which is Int -> [Int]
reachable (S n) x = concat (map (reachable n) (oneStep x))




--function to pull head and tail from a list?
