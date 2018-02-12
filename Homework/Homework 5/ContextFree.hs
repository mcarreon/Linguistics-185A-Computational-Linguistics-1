module ContextFree where

------------------------------------------------------------------------------------

data Cat = S | NP | VP | V | D | N | PP | P | IV | TV | RC deriving (Show,Eq)

data StrucDesc  = Binary Cat StrucDesc StrucDesc
                | Unary Cat StrucDesc
                | Leaf Cat String
                deriving Show

data GrammarRule    = BinaryStep Cat Cat Cat
                    | UnaryStep Cat Cat
                    | End Cat String
                    deriving Show

type Address = [Int]

grammar1 =  [   -- S rules
                BinaryStep S NP VP,
                -- VP rules
                BinaryStep VP V NP,
                BinaryStep VP V S,
                BinaryStep VP VP PP,
                UnaryStep  VP V,
                -- PP rules
                BinaryStep PP P NP,
                -- D rules
                End D "the",
                End D "a",
                -- N rules
                End N "cat",
                End N "dog",
                -- NP rules
                BinaryStep NP D N,
                End NP "John",
                End NP "Mary",
                -- V rules
                End V "left",
                End V "thinks",
                -- P rules
                End P "with"
            ]

grammar2 = [    BinaryStep S NP IV,
                BinaryStep NP N RC,
                UnaryStep NP N,
                BinaryStep RC NP TV,
                End N "dogs",   End N "cats",
                End IV "chase", End IV "sleep",
                End TV "chase"
            ]

sd1 = Binary S (Leaf NP "John") (Binary VP (Leaf V "left") (Leaf NP "Mary"))

sd2 = Binary S (Leaf NP "John")
               (Binary VP (Unary VP (Leaf V "left")) 
                          (Binary PP (Leaf P "with") (Binary NP (Leaf D "a") (Leaf N "cat")))
               )

sd3 = Binary S (Binary NP (Leaf D "the") (Leaf N "dog")) (Binary VP (Leaf V "thinks") sd1)

sd4 = Binary S (Binary NP (Leaf D "the") (Leaf N "dog")) (Binary VP (Leaf V "thinks") sd2)

sd5 = Binary S (Binary NP (Leaf N "dogs") (Binary RC (Unary NP (Leaf N "dogs")) (Leaf TV "chase"))) (Leaf IV "chase")

------------------------------------------------------------------------------------

pf :: StrucDesc -> [String]
pf (Binary c sd1 sd2) = pf sd1 ++ pf sd2
pf (Unary c sd) = pf sd
pf (Leaf c s) = [s]

leftmostLeaf :: StrucDesc -> String
leftmostLeaf (Leaf c s) = s
leftmostLeaf (Unary c sd) = leftmostLeaf sd
leftmostLeaf (Binary c sd1 sd2) = leftmostLeaf sd1

categoryOf :: StrucDesc -> Cat
categoryOf (Leaf c s) = c
categoryOf (Unary c sd) = c
categoryOf (Binary c sd1 sd2) = c

wellFormed :: [GrammarRule] -> StrucDesc -> Bool
wellFormed g (Leaf c s) = elem c (enders g s)
wellFormed g (Unary c sd) = elem c (predecessorsUnary g (categoryOf sd)) && wellFormed g sd
wellFormed g (Binary c sd1 sd2) = elem c (predecessorsBinary g (categoryOf sd1, categoryOf sd2)) 
                                            && wellFormed g sd1 && wellFormed g sd2

depth :: StrucDesc -> Int
depth (Leaf c s) = 1
depth (Unary c sd) = 1 + depth sd
depth (Binary c sd1 sd2) = if (depth sd1 > depth sd2) then (1 + depth sd1) else (1 + depth sd2)

enders :: [GrammarRule] -> String -> [Cat]
enders [] x = []
enders (r:rs) x =
    case r of
    End c s -> if s == x then c : (enders rs x) else enders rs x
    UnaryStep c ch -> enders rs x
    BinaryStep c ch1 ch2 -> enders rs x

predecessorsUnary :: [GrammarRule] -> Cat -> [Cat]
predecessorsUnary [] x = []
predecessorsUnary (r:rs) x =
    case r of
    End c s -> predecessorsUnary rs x
    UnaryStep c ch -> if ch == x then (c : (predecessorsUnary rs x)) else (predecessorsUnary rs x)
    BinaryStep c ch1 ch2 -> predecessorsUnary rs x

predecessorsBinary :: [GrammarRule] -> (Cat,Cat) -> [Cat]
predecessorsBinary [] x = []
predecessorsBinary (r:rs) x =
    case r of
    End c s -> predecessorsBinary rs x
    UnaryStep c ch -> predecessorsBinary rs x
    BinaryStep c ch1 ch2 -> if (ch1,ch2) == x then (c : (predecessorsBinary rs x)) else (predecessorsBinary rs x)

-----------------------------------------------------------------
-----------------------------------------------------------------
-- IMPORTANT: Do not change anything above here.
--            Write all your code below.
-----------------------------------------------------------------
-----------------------------------------------------------------

brackets :: StrucDesc -> String
brackets (Unary c sd) = "[" ++ (brackets sd) ++ "]"
brackets (Binary c sd1 sd2) = "[" ++ (brackets sd1) ++ " " ++ (brackets sd2) ++ "]"
brackets (Leaf c s) = s

labeledBrackets :: StrucDesc -> String
labeledBrackets (Unary c sd ) = "[" ++ (show c) ++ " " ++ (labeledBrackets sd) ++ "]"
labeledBrackets (Binary c sd1 sd2) = "[" ++ (show c) ++ " " ++ (labeledBrackets sd1) ++ " " ++ (labeledBrackets sd2) ++ "]"
labeledBrackets (Leaf c s) = s

numNPs :: StrucDesc -> Int
numNPs (Unary c sd) =  if c == NP then (1 + (numNPs sd)) else (numNPs sd)
numNPs (Binary c sd1 sd2) = if c == NP then (1 + ((numNPs sd1) + (numNPs sd2))) else (numNPs sd1 + numNPs sd2)
numNPs (Leaf c s) = if c == NP then 1 else 0

numViolations :: [GrammarRule] -> StrucDesc -> Int
numViolations g (Leaf c s) = if elem c (enders g s) then 0 else 1 
numViolations g (Unary c sd) = if elem c (predecessorsUnary g (categoryOf sd)) then 0 + numViolations g sd else 1 + numViolations g sd
numViolations g (Binary c sd1 sd2) = if elem c (predecessorsBinary g (categoryOf sd1, categoryOf sd2)) then (0 + ((numViolations g sd1) + (numViolations g sd2))) else (1 + ((numViolations g sd1) + (numViolations g sd2)))

sdMap :: (String -> String) -> StrucDesc -> StrucDesc
sdMap f (Leaf c s) = Leaf c (f(s))
sdMap f (Unary c sd) = Unary c (sdMap f sd)
sdMap f (Binary c sd1 sd2) = Binary c (sdMap f sd1)(sdMap f sd2)

longestPath :: StrucDesc -> [Cat]
longestPath (Leaf c s) = [c]
longestPath (Unary c sd) = [c] ++ longestPath sd
longestPath (Binary c sd1 sd2) = 
    if (depth sd1) == (depth sd2) then ([c] ++ longestPath sd1) else
    (if (depth sd1) > (depth sd2) then ([c] ++ longestPath sd1) else ([c] ++ longestPath sd2))

allPaths :: StrucDesc -> [[Cat]]
allPaths (Leaf c s) = [[c]] 
allPaths (Unary c sd) = map (\x -> c : x)(allPaths sd)
allPaths (Binary c sd1 sd2) = (map (\x -> c : x)(allPaths sd1)) ++ (map (\n -> c : n)(allPaths sd2))

addressesOfNPs :: StrucDesc -> [Address]
addressesOfNPs = undefined
--addressesOfNPs (Unary c sd) =  
--addressesOfNPs (Binary c sd1 sd2) = 
--addressesOfNPs (Leaf c s) = 

ccommand :: Address -> Address -> Bool
ccommand = undefined

replace :: StrucDesc -> Address -> StrucDesc -> StrucDesc
replace (Binary c sd1 sd2) (n:ns) newPart = case n of
    0 -> if ns == [] then (let sd1 = newPart in Binary c sd1 sd2) else Binary c (replace sd1 ns newPart) sd2
    1 -> if ns == [] then (let sd2 = newPart in Binary c sd1 sd2) else Binary c sd1 (replace sd2 ns newPart) 
    _ -> undefined
replace (Unary c sd) (n:ns) newPart = case n of
    0 -> if ns == [] then (let sd = newPart in Unary c sd) else Unary c (replace sd ns newPart)
    1 -> undefined
    _ -> undefined
replace (Leaf c s) (n:ns) newPart = Leaf c s  

returnLeaf :: StrucDesc -> Address -> StrucDesc
returnLeaf sd [] = sd
returnLeaf (Binary c sd1 sd2) (n:ns) = case n of
    0 -> returnLeaf sd1 ns 
    1 -> returnLeaf sd2 ns
    _ -> undefined
returnLeaf (Unary c sd) (n:ns) = case n of 
    0 -> returnLeaf sd ns
    1 -> undefined
    _ -> undefined
returnLeaf (Leaf c s) (n:ns) = Leaf c s 

move :: Address -> StrucDesc -> StrucDesc
move = undefined
--move a sd = Binary S (returnLeaf sd a) sd
--move (n:ns) (Binary c sd1 sd2) = case n of
--    0 -> 
--    1 -> move ns sd2
--    _ -> undefined
--move (n:ns) (Unary c sd) = case n of
--    0 -> move ns sd
--    1 -> undefined
--    _ -> undefined
--move (n:ns) (Leaf c s) = Leaf c "t" 
--move a sd = Binary S (returnLeaf sd a) sd
