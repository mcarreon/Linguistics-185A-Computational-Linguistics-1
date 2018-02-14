--Given some set of symbol = sequences L c_ SUM*
-- s1 is equal to s2 iff all x in the universe is in SUM*
--aRa 
--aRb => bRa
--aR, bRc => aRc

--xs1 and xs2 are either both in L or both not in L

-- -> 1 (a,1)(b,1) -> (b)2 -> (b)[3](a,3)(b,3)

--takeStepsBack 
--["a"] leads to 3, as well as ["a", "a"] and however many repitions

--ba, a, and baa leads to 2,3
--bba and bbb lead to 1,2,3
--abba leads to 1,3 

--characterizing a certain pattern
--a^n b^n = {ab, a...b...}
-- n >= 1
	
	-- N     N    V     V
	--cats dogs chase meow
	--       |
	--dogs elephants kick 
	
--{dogs chase, dogs dogs chase chase}
--thinking about equivalence relation 
	--is b equiv to bb
	--s1 equiv to s2 iff
		--Vx, xs1 and xs2 either both good or both bad
		--always ask when introducing it at a spot, equivalence adds candidate ending points. 
		--not equiv because ab is good, but abb is bad
	--bb and bbb?
		--nope not equiv
		
	--FSA with 3 states can only give you 8 equiv classes
	