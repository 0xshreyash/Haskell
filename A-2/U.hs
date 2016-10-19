module U
where

import RunTM
{------------------------------------------------------
Author: Shreyash Patodia
COMP30026 Assignment 2, 2016
Question 4
-------------------------------------------------------}

{- In this question I take a similar approach to Question 3
 the only difference being that we count the aggregate of
 the number of b's and a's in comparison to the number of c's
 so we first captialize the a (State 2 to 3) and then look for a
 c. We then captialize the c and look for a or b, if we find either
 then we capitalize that and look for a c. We do this until we run out of
 either a and b or c, if we run out of the other one at the same time as
 well then i + j = k and the machine does it's job-}

                    {- 10 is the reject state
                       9 is the accept state -}
u :: TM
u  = [((1, ' '), (2, ' ', R)),
      ((2, 'a'), (3, 'A', R)),
      ((2, 'b'), (10, 'N', L)),
      ((2, 'c'), (10, 'N', L)),
      ((2, ' '), (10, 'N', L)),
      ((3, 'a'), (3, 'a', R)),
      ((3, 'b'), (4, 'b', R)),
      ((3, 'c'), (10, 'N', L)),
      ((3, ' '), (10, 'N', L)),
      ((4, 'a'), (10, 'N', L)),
      ((4, 'b'), (4, 'b', R)),
      ((4, 'c'), (5, 'C', R)),
      ((4, ' '), (10, 'N', L)),
      ((5, 'a'), (10, 'N', L)),
      ((5, 'b'), (10, 'N', L)),
      ((5, 'c'), (5, 'c', R)),
      ((5, ' '), (6, ' ', L)),
      ((6, 'a'), (7, 'A', R)),
      ((6, 'A'), (6, 'A', L)),
      ((6, 'b'), (7, 'B', R)),
      ((6, 'B'), (6, 'B', L)),
      ((6, 'c'), (6, 'c', L)),
      ((6, 'C'), (6, 'C', L)),
      ((6, ' '), (8, ' ', R)),
      ((7, 'a'), (7, 'a', R)),
      ((7, 'A'), (7, 'A', R)),
      ((7, 'b'), (7, 'b', R)),
      ((7, 'B'), (7, 'B', R)),
      ((7, 'C'), (7, 'C', R)),
      ((7, 'c'), (5, 'C', R)),
      ((7, ' '), (10, 'N', L)),
      ((8, 'C'), (8, 'C', R)),
      ((8, 'A'), (8, 'A', R)),
      ((8, 'B'), (8, 'B', R)),
      ((8, 'c'), (10, 'N', L)),
      ((8, ' '), (9, 'Y', L))]
