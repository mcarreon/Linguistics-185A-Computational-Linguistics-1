prob :: Int -> String -> Double
prob n>0 s = sumx(prob (n-1) x * trProb x s)
prob 0 s = startProb s


prob :: Numb -> String -> Double
prob Z s = startProb 
prob (S n) s = sum(prob n x * trProb x s)



= sumattion/s forward prob a s x Pr(Si = 40 | Si-1 = s) x Pr(Wi-1 = d | Si-1 = s, Si = 40)