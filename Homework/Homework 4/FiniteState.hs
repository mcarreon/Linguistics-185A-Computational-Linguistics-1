module FiniteState where

data Numb = Z | S Numb deriving Show

------------------------------------------------------------------------------------------
-- Types for our finite-state grammars

type State = Int

data GrammarRule = Step State String State | End State deriving Show

data StrucDesc = NonLast State String StrucDesc | Last State deriving Show

------------------------------------------------------------------------------------------
-- Sample grammars and structural descriptions

grammar1 :: [GrammarRule]
grammar1 = [Step 1 "the" 2,
            Step 2 "cat" 3,
            Step 2 "dog" 3,
            Step 1 "John" 3,
            Step 3 "chased" 4,
            Step 3 "admired" 4,
            Step 3 "left" 4,
            Step 3 "left" 6,
            Step 4 "the" 5,
            Step 5 "cat" 6,
            Step 5 "dog" 6,
            Step 4 "John" 6,
            End 6
            ]

-- This grammar illustrates the way FSAs can ``do more than'' bigram grammars: 
-- a bigram grammar that generates both `the student often runs' and `the students often run' 
-- would have to generate `the student often run' as well. 
-- The same point can be made by the pair `you like yourself'/`they like themselves'.
grammar2 :: [GrammarRule]
grammar2 = [Step 1 "the" 2,
            Step 2 "student" 3,
            Step 2 "students" 4,
            Step 3 "often" 5,
            Step 4 "often" 6,
            Step 5 "runs" 7,
            Step 6 "run" 7,
            Step 7 "and" 1,
            End 7]

-- This grammar illustrates a sort of ``structural ambiguity''.
grammar3 :: [GrammarRule]
grammar3 = [Step 1 "these" 2,       Step 1 "some" 2,
            Step 1 "they" 3,        Step 1 "these" 3,
            Step 2 "dogs" 3,        Step 2 "buffalo" 3,
            Step 3 "buffalo" 4,     Step 3 "damaged" 4,
            Step 4 "damaged" 4,     Step 4 "nice" 4,
            Step 4 "u  nicorns" 5,    Step 4 "stuff" 5,
            Step 5 "and" 1,
            End 5]

-- This grammar generates all strings over {a,b} that contain 
-- at least one occurrence of `bb', and does it in the 
-- nondeterministic way that allows multiple structural 
-- descriptions for a sequence like `abbabba'.
grammar4 :: [GrammarRule]
grammar4 = [Step 1 "a" 1,   Step 1 "b" 1,
            Step 1 "b" 2,
            Step 2 "b" 3,
            Step 3 "a" 3,   Step 3 "b" 3,
            End 3]

sd1 :: StrucDesc
sd1 = NonLast 1 "the" (NonLast 2 "cat" (NonLast 3 "chased" (NonLast 4 "John" (Last 6))))

sd2 :: StrucDesc
sd2 = NonLast 1 "the" (NonLast 2 "cat" (NonLast 4 "John" (Last 6)))

------------------------------------------------------------------------------------------
-- Some functions on structural descriptions and grammars

pf :: StrucDesc -> [String]
pf (Last s) = []
pf (NonLast s w sd) = w : (pf sd)

-- `successors g s w' should produce the list of states you can 
-- reach by stepping out of state s and emitting word w, 
-- according to grammar g
successors :: [GrammarRule] -> State -> String -> [State]
successors [] s w = []
successors (r:rs) s w =
    let resultFromRest = successors rs s w in
    case r of
    End x -> resultFromRest
    Step x w' y -> if x == s && w == w' then (y:resultFromRest) else resultFromRest

-- `predecessors g w s' should produce the list of states 
-- that you can step out of and reach state s, emitting word w, 
-- according to grammar g
predecessors :: [GrammarRule] -> String -> State -> [State]
predecessors [] w s = []
predecessors (r:rs) w s =
    let resultFromRest = predecessors rs w s in
    case r of
    End x -> resultFromRest
    Step x w' y -> if y == s && w == w' then (x:resultFromRest) else resultFromRest

-- `enders g' should produce the list of states you can
-- end on, according to grammar g
enders :: [GrammarRule] -> [State]
enders [] = []
enders (r:rs) =
    let resultFromRest = enders rs in
    case r of
    End x -> x : resultFromRest
    Step x w' y -> resultFromRest

firstState :: StrucDesc -> State
firstState (Last s) = s
firstState (NonLast s w sd) = s

wellFormed :: [GrammarRule] -> StrucDesc -> Bool
wellFormed g (Last s) = elem s (enders g)
wellFormed g (NonLast s w sd) = elem (firstState sd) (successors g s w) && wellFormed g sd

-- `takeSteps g s ws' should produce the list of states you can 
-- end up at if you start at state s and walk through g along some 
-- series of arcs emitting the words ws.
--      e.g. takeSteps grammar3 1 ["these","buffalo"]  ==>  [3,4]
--      e.g. takeSteps grammar3 1 ["these","buffalo","damaged"]  ==>  [4,4]
takeStepsForward :: [GrammarRule] -> State -> [String] -> [State]
takeStepsForward g s [] = [s]
takeStepsForward g s (w:ws) = concat (map (\nextState -> takeStepsForward g nextState ws) (successors g s w))

-----------------------------------------------------------------
-----------------------------------------------------------------
-- IMPORTANT: Do not change anything above here.
--            Write all your code below.
-----------------------------------------------------------------
-----------------------------------------------------------------

--Notes

--takeStepsForward 
--The crucial idea underlying the recursive line is this: the things you can reach by taking steps forward from state s and emitting w:ws, are the things you can reach by taking steps forward from any nextState in the list successors g s w and emitting ws

--takeStepsBack 
--The crucial idea underlying the recursive step here is this: the states that lead to an end state by emitting w:ws that we need to output 
    --are the states in the list predecessors g w nextState 
        --for any nextState from which you can get to an end state by emitting ws
-- the only difference is that at the core of takeStepsBack there’s a function of type State -> [State] which is mapped over a list

------------------------------------------
-- Part 1: Recognition

-- `takeStepsBack g ws' should produce the list of states from which 
-- you can walk through g along some series of arcs emitting the words 
-- ws and end up in a valid ending state.
--      e.g. takeStepsBack grammar3 ["damaged","stuff"]  ==>  [3,4]
--      e.g. takeStepsBack grammar3 ["buffalo","damaged","stuff"]  ==>  [2,3]

takeStepsBack :: [GrammarRule] -> [String] -> [State]
takeStepsBack [] s = []
takeStepsBack g ws = 
    case ws of
    [] -> (enders g)
    (w:ws') -> concat (map (\nextState -> predecessors g w nextState) (takeStepsBack g ws'))

--takeStepsBack g (w:ws) = concat (map (\nextState -> (\validState -> )(predecessors g w nextState ))(takeStepsBack g ws))

recognizeBackward :: [GrammarRule] -> State -> [String] -> Bool
recognizeBackward = undefined
--recognizeBackward g s ws = 
--    if 

recognizeForward :: [GrammarRule] -> State -> [String] -> Bool
recognizeForward = undefined

------------------------------------------
-- Part 2: Parsing

parse :: [GrammarRule] -> [String] -> [StrucDesc]
parse g = undefined

------------------------------------------
-- Part 3: Generation

generate :: [GrammarRule] -> Numb -> [StrucDesc]
generate = undefined

