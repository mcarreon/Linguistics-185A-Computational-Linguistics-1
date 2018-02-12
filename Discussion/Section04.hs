--FSA 
--can be deterministic or non deterministic 
--can go from one state to another with only one state
--every non-deterministic can be converted to a deterministic. 

--NFSA

-> (1) these -> (2) buffalo -> (3) buffalo/damaged -> (4) stuff -> ((5))
  these                                             damaged
    |                                                  |
   (3) -> (4)                                         (4)
   

    |   these buffalo damaged stuff
-----------------------------------------
1   |  {2, 3}   --      --     --
2   |    --    {3} 
3   |          {4}      {4}
4   |                   {4}    {5}
5   |


    |   these buffalo damaged stuff
-----------------------------------------
1   |    2 3   --       --     --
2 3 |    --    3 4       4
3 4 |           4        4      5
4   |                    4      5 
5   |

takeSteps :: [GrammarRule] -> State -> [String] -> [State]


