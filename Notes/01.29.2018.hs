--Monday, 29, 2018 - Week 4
--Hints on homework?--

--applyToAll = inbuilt function map
(a -> b) -> [a] -> [a] 

--no distinction between defining variable and function
x = "hello"
f = \y -> y + 2

--suppose you have a list of a's
--xs::[a1, a2, a3 ...]
--f::a -> [b] 
--map f xs :: [[b]]
--a1 -> [b1, b2, b3], a2 -> [], a3 -> [b12, b13, b14 ...]

concat(map f xs) [b11, b12, ... b31, b32, ... bn]
--applying concat to map will combine the list of lists

--shorthand for lists
case list of [] -> x
f x:xs = 

--How does finite state automata relate to bi-grams

--is there a way to write a bigram grammar the same as FSA?
sig = {a, b, c}
L(G) <= sig*

--bigram grammar
	--set of start symbols
	--set of bigrams
	--set of end symbols
	
--FSA #1
--see journal

--BG #1
	--start symbols : a, b
	--bigrams: 
		--a -> a
		--a -> b
		--b -> a
		--b -> b
	--end : a, b
	
