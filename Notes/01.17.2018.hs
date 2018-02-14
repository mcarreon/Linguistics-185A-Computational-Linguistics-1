Notes for 01/17/2018

Types

--defining Shape
	data Shape = Rock | Paper | Scissors
--compound definition
	data Result = Draw | Win Shape
		--draw
		--win paper
		--win scissors
		--win rock
	
	data Numb = Z(zero) | S(succesor) Numb 
		--could be 
		--Z
		--S Z
		--S (S Z)
		--S (S (S Z)) 
		-- ... 
		-- ...
	--if (S S) Z
		--S needs a Numb after it, but another S is not a Numb
		--if grouped S S Z
			--haskell groups (S S) Z
	
--can define boolean
	data Bool = True | False 
--can define isZero as 
	isZero = \n -> case n of {Z -> True; S n' -> False}
		--n' is just another variable, there is no meaning to it
		--in this case n' = y whereas n = x 
	isOne = \n -> case n of {Z -> False; S n' -> isZero n'}
		--in this case n' is used in evaluation of statement after S n' -> 
	lessThanTwo = \n -> case n of 
		--there are two ways n can look
			--Z -> True 
			--S n' -> case n' of 
				--Z -> True
				--S n'' -> False
		--completed it looks like 
	lessThanTwo = \n -> case n of {Z -> True; S n' -> case n' of {Z -> True; S n'' -> False}}
	

--example step through
	case (S Z) of {Z -> True; S n' -> case n' of {Z -> True; S n'' -> False}}
	=> [Z/n'](case n' of {Z -> True; S n'' -> False})
	=> case Z of {Z -> True; S n'' -> False}
	=> True
	
--uh
	let v = e1 in e2 
	--v is in e1(...v...v...) and e2 (...v...v...)
	
f = \n -> case {Z -> Z; S n' -> S (s (f n'))}
	--in f (S(S(S Z)))

--useful commands
:t shows from type to type
	

-------------------------in class example of recursion------------------------------
module Recursion where
data Numb = Z | S Numb deriving Show

--Does not contain recursion 
--declare types for functions, if function is written incorrectly and returns incorrect type, it will return type error
	--if declaration is not put in and error will not occur

isZero :: Numb -> Bool
isZero = \n -> case n of {Z -> True; S n' -> False}

isOne :: Numb -> Bool
isOne = \n -> case n of {Z -> False; S n' -> isZero n'}
--contains recursion

double :: Numb -> Numb
double = \n -> case n of {Z -> Z); S n' -> S (S (double n')}

add	:: Numb -> (Numb -> Numb)
add = \n -> (\m -> case n of {Z -> m; S n' -> S ((add n')m)}
					--Z -> m 
					--S n' -> S ((add n') m)
						--or S n' -> (add n') (S m)
						
data Shape = Rock | Paper | Scissors deriving Show
data ShapeList = Empty | NonEmpty Shape Shapelist deriving Show 

size :: ShapeList -> Numb --gives a shapelist and returns a number
size = \sl -> case sl of 
				Empty -> Z
				NonEmpty sh sl'-> S (size sl')
				
listOf :: Numb -> (Shape -> ShapeList)
listOf = \n -> (\s -> case n of 
						Z -> Empty 
						S n' -> NonEmpty s ()
					)