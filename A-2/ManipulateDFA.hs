module ManipulateDFA 
where

import DFA
import Data.Char (isDigit, isLower)
import Data.List (intersect, sort, nub, (\\))

-- Incomplete stubs added by Shreyash Patodia
-- For COMP30026: Models of Computation
-- Assignment 2, 2016

{------------------------------------------------------------------------
    A skeleton Haskell script for COMP30026 Asg2, 2016.

    It consists of a bunch of stubs, mainly function type signatures.

    Harald Sondergaard
    The University of Melbourne
    September 2016
------------------------------------------------------------------------}


--  Keep lists sorted and without duplicates.

tidy :: Ord a => [a] -> [a]
tidy xs
  = nub (sort xs)


--  Calculate the set of reachable states in a given DFA.

reachable :: DFA -> [State]
reachable (states, alphabet, delta, start_state, accept_states)
  = new
    where
      (old, new) = until stable explore ([], [start_state])
      explore (old_reach, cur_reach) = (cur_reach, expand cur_reach)
      expand reach = tidy (reach ++ successors reach)
      successors reach = [y | ((x,_),y) <- delta, x `elem` reach]
      stable (xs, ys) = xs == ys


--  Calculate the set of generating states in a given DFA.

generating :: DFA -> [State]
generating (states, alphabet, delta, start_state, accept_states)
  = new
    where
      (old, new) 
        = until stable explore ([], accept_states)   
      explore (old_gen, cur_gen) 
        = (cur_gen, expand cur_gen)
      expand gen 
        = tidy (gen ++ predessors gen)
      predessors gen 
        = [y | ((y, _), x) <- delta, x `elem` gen]
      stable (xs, ys)
        = xs == ys          
                 


--  Trim a DFA, that is, keep only reachable, generating states
--  (the start state should always be kept).  

trim :: DFA -> DFA
trim (states, alphabet, delta, start_state, accept_states)
  = (useful_states, alphabet, useful_delta, start_state, new_accept_states)
    where 
      gen_states 
        = generating (states, alphabet, delta, start_state, accept_states)
      gen_delta
        = [gen_transition | gen_transition <- delta,
            fst (fst gen_transition) `elem` gen_states,
            snd gen_transition `elem` gen_states]
      useful_states  
        = reachable (gen_states, alphabet, gen_delta, start_state, accept_states)
      useful_delta 
        = [useful_trans | useful_trans <- gen_delta,
            fst (fst useful_trans) `elem` useful_states,
            snd useful_trans `elem` useful_states]
      new_accept_states
        = intersect accept_states useful_states
    





    --[useful_state | 
    --- useful_state <- reach (states, alphabet, delta, start_state, accept_states)
    --`and` 
    --useful_state <- generating (states, alphabet, delta, start_state, accept_states)
    --]
    --useful_delta = [((state_from, sym), state_to] |
    --state_from <- useful_state
    --`and`
    --state_to <- useful_state
    




-------------------------------------------------------------------------

--  Complete a DFA, that is, make all transitions explict.  For a DFA,
--  the transition function is always understood to be total.

complete :: DFA -> DFA
complete (states, alphabet, delta, start_state, accept_states)
  | already_complete
      = (states, alphabet, delta, start_state, accept_states)
  | otherwise
      = (all_states, alphabet, comp_delta, start_state, accept_states)  
    where 
      already_complete = (all_transitions == map fst delta)
      new_state
       = maximum states + 1
      all_transitions 
        = [(state, sym) |
            state <- states,
            sym <- alphabet]
      all_states
       = states ++ [new_state]
      all_perms 
        = [(state, sym) |
            state <- all_states,
            sym <- alphabet]
      required_delta_for
        = tidy all_perms \\ map fst delta
      to_reject = states \\ accept_states
      comp_delta
        = [((rjct, sym), new_state) | 
            (rjct, sym) <- required_delta_for,
            r <- to_reject,
            r == rjct] ++
            [((acpt, sym), acpt) |
            (acpt, sym) <- required_delta_for]
            ++ delta




      {-complete_delta 
        = tidy delta ++ explicit_transitions
      explicit_transitions
        = tidy [((state, symbol), state) | 
            state <- states,
            symbol <- alphabet,
            (st, sym) <- required_delta,
            state == st,
            symbol == sym]
      all_perms 
        = [(state, sym) |
            state <- states,
            sym <- alphabet]
      required_delta
        = tidy all_perms \\ map fst delta-}
 

            


-------------------------------------------------------------------------

--  Systematically replace the names of states in a DFA with 1..n.

normalise :: DFA -> DFA
normalise (states, alphabet, delta, start_state, accept_states)
  = (allowed_vals, alphabet, norm_delta, norm_start_state, norm_accept_states)
  where
    max_val = length states
    allowed_vals
      = [1..max_val]
    allowed_vals_used
      = intersect allowed_vals states
    not_used_vals
      = tidy allowed_vals \\ allowed_vals_used
    -- Finding the values to be replace and what to replace them with
    replace_by 
      = replace states not_used_vals max_val
    replace [] [] max_val = []
    replace states [] max_val = []
    replace [] not_used_vals max_val = []
    replace (st:sts) (poss_st:poss_sts) max_val
      | st <= max_val = [(st, st)] ++ replace sts (poss_st:poss_sts) max_val
      | otherwise = [(st, poss_st)] ++ replace sts poss_sts max_val
    -- Not a problem even if the start state is not > max_val since
    -- we have things like (1, 1) in replace_by as well
    norm_start_state 
      = head [n_s_st | (x, n_s_st) <- replace_by,  
                x == start_state] 
    norm_delta
      = [((sf, sym), st) | ((x, symbol), y) <- delta,
          (u, sf) <- replace_by,
          (v, st) <- replace_by,
          sym <- alphabet,
          u == x,
          v == y,
          symbol == sym]  
    norm_accept_states
      = [norm_acpt_st |
          (x, norm_acpt_st) <- replace_by,
          y <- accept_states,
          x == y] 
-------------------------------------------------------------------------

--  To complete and then normalise a DFA:

full :: DFA -> DFA
full dfa
  = normalise(complete dfa)


--  For a given DFA d, generate a DFA d' so that the languages of d
--  and d' are complementary.

complement :: DFA -> DFA
complement (states, alphabet, delta, start_state, accept_states)
  = complete (states, alphabet, delta, start_state, states \\ accept_states)

    



-------------------------------------------------------------------------

--  Given DFAs d1 and d', generate a DFA for the intersection of the
--  languages recognised by d1 and d2.

prod :: DFA -> DFA -> DFA
prod dfa1 dfa2
  = dfa1




-------------------------------------------------------------------------

--  Here is an example (trimmed) DFA; it recognises a*ab*c*

dex :: DFA 
dex 
  = ([0,1,2,3], "abc", t1, 0, [1,2,3])
    where 
      t1 = [ ((0,'a'), 1)
           , ((1,'a'), 1)
           , ((1,'b'), 2)
           , ((1,'c'), 3)
           , ((2,'b'), 2)
           , ((2,'c'), 3)
           , ((3,'c'), 3)
           ]

-------------------------------------------------------------------------

