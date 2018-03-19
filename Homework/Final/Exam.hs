module Exam where

import Control.Monad

import qualified Data.Map as Map

------------------------------------------------------------------------------------
-- We import some things from ExamProbCFG and ExamProbFSA.

-- The things listed in the parentheses in these import lines are the things that 
-- are being imported from the other files.

-- The things imported on this first line can be referred to directly. The extra 
-- parentheses and dots after GrammarRule, StrucDesc and Cat say that the constructors 
-- for these types, i.e. Binary and Leaf and S and NP and so on, are also imported.
import ExamProbCFG (ProbCFG, GrammarRule(..), StrucDesc(..), Cat(..), pcfg1, pcfg2, pcfg3)
-- But to use the things imported on this second line we must put 'CFG.' in front 
-- of the name, e.g. we will write 'CFG.trProb' and 'CFG.endProb'.
import qualified ExamProbCFG as CFG (trProb, endProb, allCats)

-- And similarly, the things imported on this first line can be referred to directly ...
import ExamProbFSA (ProbFSA, State, pfsa1, pfsa2, pfsa3, pfsa4)
-- ... and the things imported on this second line must be referred to with 
-- 'FSA.' added to the front of their name.
import qualified ExamProbFSA as FSA (trProb, emProb, endProb, allStates)

-- Notice that we have imported GrammarRule and StrucDesc types from 
-- ExamProbCFG, but not from ExamProbFSA (even though we have seen 
-- types with those names in the context of FSAs as well).

------------------------------------------------------------------------------------
-- You should feel free to IGNORE THIS SECTION. These definitions are used to 
-- implement the table-based memoization of computed values in a modular fashion 
-- that doesn't get in the way.

type Table c v = Map.Map ([String],c) v
data TableBased c v a = MkTableBased (Table c v -> (a, Table c v))
type TableBasedFromCFG a = TableBased Cat a a
type TableBasedFromFSA a = TableBased State a a

instance Functor (TableBased c v) where
    fmap = liftM
instance Applicative (TableBased c v) where
    pure x = MkTableBased (\n -> (x,n))
    (<*>) = ap
instance Monad (TableBased c v) where
    (MkTableBased fa) >>= k =
        MkTableBased $ \n ->
            let (a,n') = fa n in
            let (MkTableBased fb) = k a in
            let (b,n'') = fb n' in
            (b,n'')

lift :: ([a] -> a) -> [TableBased c a a] -> TableBased c a a
lift f xs = liftM f (sequence xs)

tryRetrieveElse :: (Ord c) => ([String],c) -> TableBased c a a -> TableBased c a a
tryRetrieveElse (ws,c) v =
    let yield x = MkTableBased (\tbl -> (x, Map.insert (ws,c) x tbl)) in
    MkTableBased (\tbl -> (Map.lookup (ws,c) tbl, tbl)) >>= (\r ->
        case r of
        Just x -> pure x
        Nothing -> v >>= yield
    )

goFromEmptyTable :: TableBased c a a -> a
goFromEmptyTable (MkTableBased f) = fst (f Map.empty)

------------------------------------------------------------------------------------
-- These are the generic functions for computing values from an FSA and from a CFG.
-- You should be able to see the outlines of familiar patterns here, in amongst 
-- various details that you can ignore.

genericFSA :: BundleForFSA a -> ProbFSA -> [String] -> State -> a
genericFSA b pfsa ws st =
    let (endVal, trVal, emVal, allOf, oneOf) = b in
    let worker output st =
            tryRetrieveElse (output,st) (
                case output of
                [] -> pure (endVal pfsa st)
                (w:ws) ->   let contributionOf next = lift allOf [pure (trVal pfsa st next), 
                                                                  pure (emVal pfsa (st,next) w), 
                                                                  worker ws next]
                            in
                            lift oneOf [contributionOf next | next <- FSA.allStates pfsa]
            )
    in
    goFromEmptyTable (worker ws st)

genericCFG :: BundleForCFG a -> ProbCFG -> [String] -> Cat -> a
genericCFG b pcfg ws c = 
    let (endVal, trVal, allOf, oneOf) = b in
    let worker output c =
            tryRetrieveElse (output,c) (
                case output of
                [] -> undefined
                [w] -> pure (endVal pcfg c w)
                _ -> 
                    let contributionOf c1 c2 i = lift allOf [pure (trVal pcfg c (c1,c2)), 
                                                             worker (take i output) c1, 
                                                             worker (drop i output) c2]
                    in
                    lift oneOf [contributionOf c1 c2 i | c1 <- CFG.allCats pcfg, 
                                                         c2 <- CFG.allCats pcfg, 
                                                         i <- [1 .. length output - 1]]
            )
    in
    goFromEmptyTable (worker ws c)

------------------------------------------------------------------------------------

type BundleForFSA a = ( ProbFSA -> State -> a,                          -- ending values
                        ProbFSA -> State -> State -> a,                 -- transition values
                        ProbFSA -> (State,State) -> String -> a,        -- emission values
                        [a] -> a,                                       -- combine such that all happen
                        [a] -> a                                        -- combine such that one happens
                      )

type BundleForCFG a = ( ProbCFG -> Cat -> String -> a,                  -- ending values
                        ProbCFG -> Cat -> (Cat, Cat) -> a,              -- transition values
                        [a] -> a,                                       -- combine such that all happen
                        [a] -> a                                        -- combine such that one happens
                      )

-- This type may be useful for debugging, but you can ignore it if you like.
data EventFromCFG = EndEvent Cat String | StepEvent Cat (Cat,Cat) 
                  | AllOf [EventFromCFG] | OneOf [EventFromCFG] | None
                    deriving (Show,Eq)

backwardBundleFSA :: BundleForFSA Double
backwardBundleFSA = (FSA.endProb, FSA.trProb, FSA.emProb, product, sum)

recognizeBundleFSA :: BundleForFSA Bool
recognizeBundleFSA =
    let probToBool p = p > 0 in
    (   \g -> \st -> probToBool (FSA.endProb g st),
        \g -> \st1 -> \st2 -> probToBool (FSA.trProb g st1 st2),
        \g -> \(st1,st2) -> \w -> probToBool (FSA.emProb g (st1,st2) w),
        and,
        or
    )

insideBundleCFG :: BundleForCFG Double
insideBundleCFG = (CFG.endProb, CFG.trProb, product, sum)

recognizeBundleCFG :: BundleForCFG Bool
recognizeBundleCFG = 
    let probToBool p = p > 0 in 
    (   \g -> \cat -> \w -> probToBool (CFG.endProb g cat w),
        \g -> \cat -> \(cat1, cat2) -> probToBool (CFG.trProb g cat (cat1, cat2)),
        and,
        or
    )

viterbiBundleFSA :: BundleForFSA Double
viterbiBundleFSA = (FSA.endProb, FSA.trProb, FSA.emProb, product, maximum)

viterbiBundleCFG :: BundleForCFG Double
viterbiBundleCFG = (CFG.endProb, CFG.trProb, product, maximum)

countBundleFSA :: BundleForFSA Int
countBundleFSA = 
    let probToCount p = if p == 0 then 0 else 1 in 
    (   \g -> \st -> probToCount (FSA.endProb g st), 
        \g -> \st1 -> \st2 -> probToCount (FSA.trProb g st1 st2), 
        \g -> \(st1,st2) -> \w -> probToCount (FSA.emProb g (st1,st2) w), 
        product, 
        sum
    )

countBundleCFG :: BundleForCFG Int
countBundleCFG = 
    let probToCount p = if p == 0 then 0 else 1 in 
    (   \g -> \cat -> \w -> probToCount (CFG.endProb g cat w),
        \g -> \cat -> \(cat1, cat2) -> probToCount (CFG.trProb g cat (cat1, cat2)),
        product,
        sum
    )

productList :: [Double] -> [Double] -> [Double]
productList x y = [product (x ++ y)] 

probToProb1 :: Double -> [Double]
probToProb1 p = if p > 0 then [p] else []

probToEmProb :: Double -> [Double]
probToEmProb p = [p] 

test :: (ProbCFG -> Cat -> String -> Double) -> [Double]
test = undefined

probsBundleFSA :: BundleForFSA [Double]
probsBundleFSA = 
    let probToProb p = if sum p > 0 then p else [] in
    (   \g -> \st -> probToProb ([FSA.endProb g st] ), 
        \g -> \st1 -> \st2 -> probToProb ([FSA.trProb g st1 st2]), 
        \g -> \(st1,st2) -> \w -> probToProb ([FSA.emProb g (st1,st2) w]), 
        foldr1 productList, 
        concat
    )

probsBundleCFG :: BundleForCFG [Double]
probsBundleCFG = undefined
--    let probToProb p = if product p > 0 then p else [] in
--    (   \g -> \cat -> \w -> probToProb ([CFG.endProb g cat w]), 
--        \g -> \cat -> \(cat1, cat2) -> probToProb ([CFG.trProb g cat (cat1, cat2)]),  
--        foldr1 productList, 
--    )

derivCFG :: Double -> Bool
derivCFG p = if p > 0 then True else False

concatDeriv :: [[GrammarRule]] -> [[GrammarRule]] -> [[GrammarRule]]
concatDeriv = undefined 
--concatDeriv [] [] = [[]]
--concatDeriv [x] [] = [x]
--concatDeriv [] [y] = [y]
--concatDeriv [x] [y] = [x ++ y]

derivationBundleCFG :: BundleForCFG [[GrammarRule]]
--derivationBundleCFG = undefined
derivationBundleCFG = 
    (   \g -> \cat -> \w -> if derivCFG (CFG.endProb g cat w) == True then [[End cat w]] else [],
        \g -> \cat -> \(cat1, cat2) -> if derivCFG (CFG.trProb g cat (cat1,cat2)) == True then [[Step cat (cat1,cat2)]] else [],
        foldr1 concatDeriv,
        concat 
    )

probDerivBundleCFG :: BundleForCFG [(Double,[GrammarRule])]
probDerivBundleCFG = undefined

countVPsBundleCFG :: BundleForCFG [Int]
countVPsBundleCFG = undefined

------------------------------------------------------------------------------------



convert :: [GrammarRule] -> GrammarRule 
convert (x:[]) = x

{-

takeHelp :: [GrammarRule] -> Int 
takeHelp [] = 0
takeHelp (r:[]) = 0
takeHelp (r:rn) = case r of 
    End fn fs -> let y = convert (take 1 rn) in
        case y of
            End zn zs -> 2
            Step zn (zx,zy) -> takeHelp rn
    Step fn fs -> 1 + takeHelp rn 

-}

leftmostCheckStack :: [GrammarRule] -> [Cat] -> Bool
leftmostCheckStack [] [] = True
leftmostCheckStack (g:gs) a = case a of 
    [] -> case g of 
        Step fn (fx, fy) -> leftmostCheckStack gs [fx, fy] 
        _ -> False
    _ -> case g of
        Step fn (fx, fy) -> if fn == head a then leftmostCheckStack gs ([fx, fy] ++ drop 1 a) else False
        End fn fs -> if fn == head a then leftmostCheckStack gs (drop 1 a) else False

leftmostCheck :: [GrammarRule] -> Bool 
leftmostCheck g = (leftmostCheckStack g [])

{-leftmostCheck [] = False
leftmostCheck (r:[]) = True 
leftmostCheck (r:rn) = case r of 
    Step fn (fx, fy) -> let x = convert (take 1 rn) in  
        case x of 
            Step xn (xx, xy) -> if xn == fx 
                then let y = convert (take 1 (drop ((takeHelp rn) + 1) rn)) in case y of 
                    Step yn (yx, yy) -> if yn == fy then leftmostCheck rn else False
                    End yn ys -> if yn == fy then leftmostCheck rn else False
                else False
            End xn xs -> if xn == fx 
                then let y = convert (take 1 (drop 1 rn)) in case y of
                    Step yn (yx, yy) -> if yn == fy then leftmostCheck rn else False
                    End yn ys -> if yn == fy then leftmostCheck rn else False
                else False 
    End fn fs -> leftmostCheck rn
-}


{-leftmostCheck (r:rn) = case r of 
    Step fn (fx, fy) -> let x = convert (take 1 rn) in let y = convert (take 1 (drop 1 rn)) in 
        case x of 
            Step xn (xx, xy) -> if xn == fx 
                then case y of 
                    Step yn (yx, yy) -> if yn == fy then leftmostCheck rn else False
                    End yn ys -> if yn == fy then leftmostCheck rn else False
                else False
            End xn xs -> if xn == fx 
                then case y of
                    Step yn (yx, yy) -> if yn == fy then leftmostCheck rn else False
                    End yn ys -> if yn == fy then leftmostCheck rn else False
                else False 
    End fn fs -> leftmostCheck rn
-}
data Result = No | Yes StrucDesc deriving Show


insideStruc :: [GrammarRule] -> Int
insideStruc [] = 0
insideStruc (r:[]) = 0
insideStruc (r:rn) = case r of 
    End fn fs -> let y = convert (take 1 rn) in
        case y of
            End zn zs -> 2
            Step zn (zx,zy) -> insideStruc rn
    Step fn fs -> 1 + insideStruc rn 


--gramToStrucStack :: [GrammarRule] -> [StrucDesc] -> StrucDesc 
--gramToStrucStack [] a = Leaf VP "saw"
--gramToStrucStack (g:gs) a = case g of 
--    Step n (x, y) -> gramToStrucStack gs (a ++ [Binary n (x, y)])
 --   End n s -> gramToStrucStack gs (a ++ [Leaf n s])




gramToStruc :: [GrammarRule] -> StrucDesc
gramToStruc ((End cat s):[]) = Leaf cat s
gramToStruc (r:rs) = case r of 
    Step n (x, y) -> let next = convert (take 1 rs) in case 
        next of 
            Step fn (fx, fy) -> Binary n (gramToStruc (take (insideStruc rs) rs)) (gramToStruc (drop (insideStruc rs) rs))
            End n s -> Binary n (gramToStruc rs) (gramToStruc (drop 1 rs)) 
    End n s -> Leaf n s


leftmostToSD :: [GrammarRule] -> Result
leftmostToSD g = if leftmostCheck g == False 
    then No 
    else Yes (gramToStruc g) 
