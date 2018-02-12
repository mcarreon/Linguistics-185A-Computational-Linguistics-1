Marlo Carreon 
Linguistics 185A
1/16/2018
504325272

Assignment #1

n = 1 
f = \s -> case s of {Rock -> 334; Paper -> 138; Scissors -> 99} 
g = \z -> z + 4 
whatItBeats = \s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper}

1. let x = 4 + 5 in (3 * x)
=⇒	3 * (4 + 5)																							let reduction
=⇒	3 * (9)																								arithmatic
=⇒	27																									arithmatic

2. (\x -> 3 * x) (4 + 5)
=⇒	3 * (4 + 5)																							lambda reduction
=⇒	3 * (9)																								arithmatic
=⇒	27																									arithmatic

3. ((\x -> (\y -> x + (3 * y))) 4) 1
=⇒	((\x -> (x + (3 * 4)) 1																				lambda reduction	
=⇒	(1 + (3 * 4))																						lambda reduction 
=⇒	1 + 12																								arithmatic
=⇒	13																									arithmatic

4. let x = 4 in (let y = 1 in (x + (3 * y)))
=⇒	let x = 4 in (x + (3 * 1))																			let reduction
=⇒	4 + (3 * 1)																							let reduction
=⇒	4 + 3 																								arithmatic
=⇒	7																									arithmatic

5. let x = 4 in (let y = 1 + x in (x + (3 * y)))
=⇒	let x = 4 in (x + (3 * (1 + x)))																	let reduction
=⇒	4 + (3 * (1 + 4))																					let reduction
=⇒	4 + (3 * 5)																							arithmatic
=⇒	4 + 15																								arithmatic
=⇒	19																									arithmatic

6. ((\x -> (\y -> x + (3 * x))) 4) 1
=⇒	((\x -> [4/y]x + (3 * x))) 1																		lambda reduction
=⇒	[4/y]1 + (3 * 1)																					lambda reduction
=⇒	[4/y]1 + 3																							arithmatic	
=⇒	[4/y]4																								arithmatic

7. ((\x -> (\y -> y + (3 * y))) 4) 1
=⇒	((\x -> 4 + (3 * 4))) 1																				lambda reduction
=⇒	[1/x]4 + (3 * 4)																					lambda reduction
=⇒	[1/x]4 + 12																							arithmatic
=⇒	[1/x]16																								arithmatic

8. (\y -> y + ((\y -> 3*y) 4)) 5
=⇒	(\y -> y + (3 * 4)) 5																				lambda reduction
=⇒	5 + (3 * 4)																							lambda reduction
=⇒	5 + 12																								arithmatic
=⇒	17																									arithmatic

9. (\y -> ((\y -> 3*y) 4) + y) 5
=⇒	(\y -> (3 * 4) + y) 5																				lambda reduction
=⇒	(3 * 4) + 5																							lambda reduction
=⇒	12 + 5																								arithmatic
=⇒	17																									arithmatic

10. (\x -> x * (let x = 3*2 in (x + 7)) + x) 4
=⇒	(\x -> x * ((3 * 2) + 7) + x) 4																		let reduction
=⇒	4 * (((3 * 2) + 7) + 4)																				lambda reduction
=⇒	4 * ((6 + 7) + 4)																					arithmatic
=⇒	4 * (13 + 4)																						arithmatic
=⇒	4 * 17																								arithmatic
=⇒	68																									arithmatic
 
11. g ((let x = 4 in (\y -> x + y)) 2)
=⇒	g ((\y -> 4 + y) 2)																					let reduction
=⇒	g (4 + 2)																							lambda reduction
=⇒	\z -> z + 4 (4 + 2)																					substitution from file
=⇒	(\z -> z + 4 (6))																					arithmatic
=⇒	6 + 4																								lambda reduction
=⇒	10																									arithmatic

12. let x = 5 in (\z -> x * z)
=⇒	(\z -> 5 * z)																						let reduction
=⇒	\z -> 5 * z						   																	closed lambda expression

13. f ((\fn -> fn Rock) (\x -> whatItBeats x))
=⇒	f ((\x -> whatItBeats x) Rock)																		lambda reduction
=⇒	f ((\s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper}) Rock)						substitution from file
=⇒	f (case Rock of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper})								lambda reduction
=⇒	f (Scissors)																						case reduction
=⇒	(\s -> case s of {Rock -> 334; Paper -> 138; Scissors -> 99}) Scissors								substitution from file
=⇒	case Scissors of {Rock -> 334; Paper -> 138; Scissors -> 99}										case reduction
=⇒	99																									case reduction

14. ((\f -> (\x -> f (f x))) whatItBeats) Paper
=⇒	((\f -> f (f whatItBeats))) Paper																	lambda reduction
=⇒	(f whatItBeats) Paper																				lambda reduction

15. whatItBeats (case Paper of {Rock -> Paper; Paper -> Rock; Scissors -> Scissors})
=⇒	whatItBeats (Rock)																					case reduction
=⇒	\s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper} Rock							substitution from file
=⇒	case Rock of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper}									lambda reduction
=⇒	Scissors																							case reduction

16. (case (Win Rock) of {Draw -> whatItBeats; Win z -> (\s -> Scissors)}) Paper
=⇒	[Rock/z](\s -> Scissors) Paper																		case reduction
=⇒	[Rock/z][Paper/s]Scissors																			lambda reduction

17. case (Win (whatItBeats Rock)) of {Draw -> n; Win x -> (n + f x)}
=⇒	case (Win (\s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper} Rock)) of {Draw -> n; Win x -> (n + f x)}			substitution from file
=⇒	case (Win (case Rock of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper})) of {Draw -> n; Win x -> (n + f x)}					lambda reduction
=⇒	case (Win Scissors) of {Draw -> n; Win x -> (n + f x)}																				case reduction
=⇒	case (Win Scissors) of {Draw -> n; Win x -> (n + (\s -> case s of {Rock -> 334; Paper -> 138; Scissors -> 99} x))}					substitution from file
=⇒	case (Win Scissors) of {Draw -> 1; Win x -> (1 + (\s -> case s of {Rock -> 334; Paper -> 138; Scissors -> 99} x))}					substitution from file
=⇒	[Scissors/x](1 + (\s -> case s of {Rock -> 334; Paper -> 138; Scissors -> 99} x))													lambda reduction
=⇒	1 + (\s -> case s of {Rock -> 334; Paper -> 138; Scissors -> 99} Scissors)															lambda reduction
=⇒	1 + (case Scissors of {Rock -> 334; Paper -> 138; Scissors -> 99})																	case reduction
=⇒	1 + 99																																arithmatic
=⇒	100																																	arithmatic

18. let y = 2 in (case (Win (whatItBeats Rock)) of {Draw -> n; Win y -> (n + f y)} + y)
=⇒	let y = 2 in (case (Win (\s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper} Rock)) of {Draw -> n; Win y -> (n + f y)} + y)			substitution from file
=⇒	let y = 2 in (case (Win (case Rock of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper} )) of {Draw -> n; Win y -> (n + f y)} + y)					lambda reduction
=⇒	let y = 2 in (case (Win Scissors) of {Draw -> n; Win y -> (n + f y)} + y)																				case reduction
=⇒	let y = 2 in [Scissors/y](n + f y)} + y																													lambda reduction
=⇒	let y = 2 in (n + f Scissors) + y																														substitution from file	
=⇒	let y = 2 in (n + (\s -> case s of {Rock -> 334; Paper -> 138; Scissors -> 99} Scissors)) + y															lambda reduction
=⇒	let y = 2 in (n + (case Scissors of {Rock -> 334; Paper -> 138; Scissors -> 99})) + y																	case reduction
=⇒	let y = 2 in (n + 99) + y																																substitution from file
=⇒	let y = 2 in (1 + 99) + y																																arithmatic
=⇒	let y = 2 in 100 + y																																	let reduction
=⇒	100 + 2																																				 	arithmatic
=⇒	102																																						arithmatic

	

\s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper}
\s -> case s of {Rock -> 334; Paper -> 138; Scissors -> 99}