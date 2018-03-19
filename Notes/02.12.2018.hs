parse :: [Grammar Rule] -> [String] -> [StrucDesc]
parse g [] = map (\s -> Last s) (enders g)
parse g (w:ws) = concat (map (StrucDesc -> [StrucDesc])(parse g ws))

takeStepsBack ::[Grammar Rule] -> [String] -> [State]
takeStepsBack g [] = enders g
takeStepsBack g (w:ws) = concat (map (\state -> predecessors g w state)(takeStepsBack g ws))

predecessors :: [Grammar Rule] -> String -> (State -> [State])
predecessors = undefined


--unacceptable vs ungrammatical 
--When a sentence is unacceptable its usually ungrammatical
--
--
--
--
--
--
--
--
--
--
--
--

---------------Context-free parsing schemas-----------------
-- x -> w
-- x -> y1.....yn

type Config = ([Cat], [String])
			--(State, [String])