module D
where

import RunDFA

{------------------------------------------------------
Author: Shreyash Patodia
COMP30026 Assignment 2, 2016
Question 1
-------------------------------------------------------}

d :: DFA
d = ([1,2,3,4,5,6,7], "abc", delta, 1, [4])
    where
      delta
        = [((1, 'a'), 2), ((1, 'b'), 5), ((1, 'c'), 5),
           ((2, 'a'), 5), ((2, 'b'), 3), ((2, 'c'), 5),
           ((3, 'a'), 4), ((3, 'b'), 7), ((3, 'c'), 6),
           ((4, 'a'), 6), ((4, 'b'), 7), ((4, 'c'), 6),
           ((5, 'a'), 5), ((5, 'b'), 5), ((5, 'c'), 5),
           ((6, 'a'), 6), ((6, 'b'), 7), ((6, 'c'), 6),
           ((7, 'a'), 4), ((7, 'b'), 7), ((7, 'c'), 6)]
                {-- State 1 is the initial state
                    State 5 is the reject state
                    State 4 is the accept state
                We start at state 1, if we get an a
                followed by a b then we go to stage 3
            then if we get an a we go to the accept state
            if we have a sequence longer than 3 then we will
             continue to go to the accept state everytime we
            get a ba and move out if we enounter something
             else (meaning that the string had not ender) --}
