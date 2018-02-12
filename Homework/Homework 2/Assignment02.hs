module Assignment02 where

data Numb = Z | S Numb deriving Show

add :: Numb -> Numb -> Numb
add m n = case n of {Z -> m; S n' -> S (add m n')}

one, two, three, four, five, six :: Numb
one = S Z
two = S one
three = S two
four = S three
five = S four
six = S five

data NumbList = EmptyNL | NonEmptyNL Numb NumbList deriving Show

list0, list1, list2 :: NumbList
list0 = NonEmptyNL one (NonEmptyNL two (NonEmptyNL three EmptyNL))
list1 = NonEmptyNL four (NonEmptyNL Z (NonEmptyNL two EmptyNL))
list2 = NonEmptyNL six (NonEmptyNL one (NonEmptyNL three (NonEmptyNL four EmptyNL)))

-----------------------------------------------------------------
-----------------------------------------------------------------
-- IMPORTANT: Do not change anything above here.
--            Write all your code below.
-----------------------------------------------------------------
-----------------------------------------------------------------

--Recursive functions on the Numb type
--A. Computes the sum of all the numbers less than or equal to the given number
sumUpTo :: Numb -> Numb
sumUpTo n = case n of 
             Z -> Z
             S n' -> add n (sumUpTo n')

--B. returns True if two numbers given are equal, False otherwise
equal :: Numb -> (Numb -> Bool)
equal m n = case n of 
             S n' -> case m of
                     S m' -> equal m' n';
                     Z -> False
             Z -> case m of 
                 Z -> True;
                 m -> False

--C. Computes the absolute value of the difference between two given numbers
difference :: Numb -> (Numb -> Numb)
difference m n = case n of 
                 Z -> m
                 S n' -> case m of
                             S m' -> difference m' n'
                             Z -> n 

--Recursive functions on lists of Numbs
--D. computes the total sum of the elements of the given list
total :: NumbList -> Numb
total nl = case nl of {EmptyNL -> Z; NonEmptyNL n nl' -> add n (total nl')}

--E. adds the given number to each of the elements in the given list 
incrementAll :: Numb -> (NumbList -> NumbList)
incrementAll n nl = case nl of {EmptyNL -> EmptyNL; NonEmptyNL y nl' -> NonEmptyNL(add n y)(incrementAll n nl') }

--F. lengthens the given list by putting the given number at the end of the list. 
addToEnd :: Numb -> (NumbList -> NumbList)
addToEnd n nl = case nl of {EmptyNL -> NonEmptyNL n (EmptyNL); NonEmptyNL y nl' -> NonEmptyNL y (addToEnd y nl')}

--G. Computes the last element of the given list, returns Z if empty
lastElement :: NumbList -> Numb
lastElement nl = case nl of
                     EmptyNL -> Z
                     NonEmptyNL y (EmptyNL) -> y
                     NonEmptyNL y nl' -> lastElement nl'

--H. The result of contains f list is True if there is at least one element of list on which f returns True, and false otherwise
contains :: (Numb -> Bool) -> (NumbList -> Bool)
contains f nl = case nl of
                 EmptyNL -> False
                 NonEmptyNL y nl' -> if f y then True else contains f nl'

--I. removes from the given list all the elements on which the given function returns True
remove :: (Numb -> Bool) -> (NumbList -> NumbList)
remove f nl = case nl of
                 EmptyNL -> EmptyNL
                 NonEmptyNL y nl' -> if f y then remove f nl' else NonEmptyNL y (remove f nl')

--J. produces a new list containing the elements of the two given lists combined in the order given
append :: NumbList -> (NumbList -> NumbList)
append nl ml = case nl of 
                 NonEmptyNL y nl' -> NonEmptyNL y (append nl' ml)
                 EmptyNL -> case ml of
                             EmptyNL -> EmptyNL
                             NonEmptyNL z ml' -> NonEmptyNL z (append nl ml')
--K. result of prefix n 1 is the length n prefix of 1, or if n is greater than length of 1 the result should be 1
prefix :: Numb -> (NumbList -> NumbList)
prefix n nl = case n of 
                 Z -> EmptyNL
                 S n' -> case nl of 
                             EmptyNL -> EmptyNL
                             NonEmptyNL y nl' -> NonEmptyNL y (prefix n' nl')