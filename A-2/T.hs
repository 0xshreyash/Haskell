module T
where

import RunTM
{------------------------------------------------------
Author: Shreyash Patodia
COMP30026 Assignment 2, 2016
Question 3
-------------------------------------------------------}

{-- The strategy that I adopt for this question is to
 count the number of a's and c's by captializing them in
 alternation in order to maintain a count of the numbers
 of a and c relative to each other, if we run out of a's them
 we should also run out of c's and vice versa.
 We start off by capitalizing the first A and then looking for a
 c (states 2, 3 and 4) and then we capitalize the c and go right
 till we end the string. Then we go back and look for an a (State 6), if
 there are no more a's then there should be no more c's and we check
 for that and then if we find a c then the language specification
 is not satisfied, if it does not have any c's left then the language
 is upheld. Similarly if we run out of c's we check if we have any more
 a's, if not then we accept else reject. This turning machine also makes
 sure to reject incorrect input right off the bat. We print out a N or Y
 as the output. If we have more a's then we first captialzie it and then look
 for a c. This is basically like adding 1 -1 +1 etc in order to get 0 --}

          {- 10 is the reject state, 11 is the accept state -} 
t :: TM
t = [((1, ' '), (2, ' ', R)),
     ((2, 'a'), (3, 'A', R)),
     ((2, 'b'), (13, 'b', R)),
     ((2, 'c'), (10, 'N', L)),
     ((2, ' '), (10, 'N', L)),
     ((3, 'a'), (3, 'a', R)),
     ((3, 'b'), (4, 'b', R)),
     ((3, 'c'), (10, 'N', L)),
     ((3, ' '), (10, 'N', L)),
     ((4, 'a'), (10, 'N', L)),
     ((4, 'b'), (10, 'N', L)),
     ((4, 'c'), (5, 'C', R)),
     ((4, 'C'), (4, 'C', R)),
     ((4, ' '), (12, ' ', L)),
     ((5, 'a'), (10, 'N', L)),
     ((5, 'b'), (10, 'N', L)),
     ((5, 'c'), (5, 'c', R)),
     ((5, ' '), (6, ' ', L)),
     ((6, 'a'), (7, 'a', L)),
     ((6, 'A'), (8, 'A', R)),
     ((6, 'b'), (6, 'b', L)),
     ((6, 'c'), (6, 'c', L)),
     ((6, 'C'), (6, 'C', L)),
     ((6, ' '), (10, 'N', L)),
     ((7, 'a'), (7, 'a', L)),
     ((7, 'A'), (2, 'A', R)),
     ((8, 'A'), (8, 'A', R)),
     ((8, 'b'), (8, 'b', R)),
     ((8, 'c'), (10, 'N', L)),
     ((8, 'C'), (8, 'C', R)),
     ((8, ' '), (11, 'Y', L)),
     ((12, 'b'), (10, 'N', L)),
     ((12, 'C'), (14, 'C', L)),
     ((13, 'a'), (10, 'N', L)),
     ((13, 'b'), (10, 'N', L)),
     ((13, 'c'), (10, 'N', L)),
     ((13, ' '), (11, 'Y', L)),
     ((14, 'a'), (10, 'N', L)),
     ((14, ' '), (11, 'Y', L)),
     ((14, 'A'), (14, 'A', L)),
     ((14, 'C'), (14, 'C', L)),
     ((14, 'b'), (14, 'b', L))]
