module ManipulateDFA
where

import DFA
import Data.Char (isDigit, isLower)
import Data.List (intersect, sort, nub, (\\))

-- Incomplete stubs added by Shreyash Patodia
-- For COMP30026: Models of Computation
-- Assignment 2, 2016

{-------------------------------------------------------------------
    A skeleton Haskell script for COMP30026 Asg2, 2016.

    It consists of a bunch of stubs, mainly function type signatures.

    Harald Sondergaard
    The University of Melbourne
    September 2016
--------------------------------------------------------------------}


--  Keep lists sorted and without duplicates.

tidy :: Ord a => [a] -> [a]
tidy xs
  = nub (sort xs)

--------------------------------------------------------------------

--  Calculate the set of reachable states in a given DFA.

reachable :: DFA -> [State]
reachable (states, alphabet, delta, start_state, accept_states)
  = new
    where
      (old, new)
        = until stable explore ([], [start_state])
      explore (old_reach, cur_reach)
        = (cur_reach, expand cur_reach)
      expand reach
        = tidy (reach ++ successors reach)
      successors reach
        = [y | ((x,_),y) <- delta, x `elem` reach]
      stable (xs, ys)
        = xs == ys

--------------------------------------------------------------------------

--  Calculate the set of generating states in a given DFA.
-- This function finds every state of the DFA that can get to the
-- final state (i.e. accept state)

generating :: DFA -> [State]
generating (states, alphabet, delta, start_state, accept_states)
  = new
    where
      -- Keep on exploring until we get a stable configuration
      (old, new)
        = until stable explore ([], accept_states)
      -- explore just expands on the current generating states
      explore (old_gen, cur_gen)
        = (cur_gen, expand cur_gen)
      -- expand just adds the predecessors since if we
      -- can get from accept to the successor then we
      -- can get to predessors
      expand gen
        = tidy (gen ++ predessors gen)
      predessors gen
        = [y | ((y, _), x) <- delta, x `elem` gen]
      -- Do it unitl old and new configurations are
      -- the same
      stable (xs, ys)
        = xs == ys


--------------------------------------------------------------------------

--  Trim a DFA, that is, keep only reachable, generating states
--  (the start state should always be kept).
--  We trim the DFA to keep only the reachable and generating state
-- of the DFA

trim :: DFA -> DFA
trim (states, alphabet, delta, start_state, accept_states)
  = (useful_states, alphabet, useful_delta, start_state,
        new_accept_states)
    where
     -- Find the generating states
      gen_states
        = generating (states, alphabet, delta, start_state,
            accept_states)
     -- Find the transitions corresponding to gen states
      gen_delta
        = [gen_transition | gen_transition <- delta,
            fst (fst gen_transition) `elem` gen_states,
            snd gen_transition `elem` gen_states]
     -- From that find reachable states to get useful ones.
      useful_states
        = reachable (gen_states, alphabet, gen_delta,
            start_state, accept_states)
     -- Transitions for the useful states
      useful_delta
        = [useful_trans | useful_trans <- gen_delta,
            fst (fst useful_trans) `elem` useful_states,
            snd useful_trans `elem` useful_states]
     -- Waiting for the new accept states
      new_accept_states
        = intersect accept_states useful_states


--------------------------------------------------------------------------
    {- Code stubs not submitted in final solution -}
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
-- Check if the DFA is already complete i.e. transitons are already
-- present
complete (states, alphabet, delta, start_state, accept_states)
  | already_complete
      = (states, alphabet, delta, start_state, accept_states)
-- Otherwise do this:
  | otherwise
      = (all_states, alphabet, comp_delta, start_state,
        accept_states)
    where
     -- Check for prior completeness
      already_complete
        = (all_transitions == map fst delta)
      -- All transition entailing the original DFA
      all_transitions
        = [(state, sym) |
            state <- states,
            sym <- alphabet]
      -- Adding a new transition to the DFA
      new_state
       = maximum states + 1
      -- The new set of states
      all_states
       = states ++ [new_state]
      -- All transtion "froms" in the new DFA
      all_perms
        = [(st, sym) |
            st <- all_states,
            sym <- alphabet]
     -- We need these unaccounted transitions
      required_delta_for
        = tidy all_perms \\ map fst delta
     -- The total delta of the complete DFA
      comp_delta
        = tidy [((st, sym), new_state) |
            (st, sym) <- required_delta_for] ++
              delta


--------------------------------------------------------------------------

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
    -- The number of states will be the max_val of the state number
    max_val = length states
    -- List of allowed state names
    allowed_vals
      = [1..max_val]
    -- Find ones that have already been used
    allowed_vals_used
      = intersect allowed_vals states
    -- Find numbers that have not been used yet
    not_used_vals
      = tidy allowed_vals \\ allowed_vals_used
    -- Finding the values to be replaced and what to replace them with
    replace_vals
      = replace states not_used_vals max_val
    -- Function to make tuples to replace the invalid values
    -- with valid values
    replace [] [] max_val = []
    replace states [] max_val = []
    replace [] not_used_vals max_val = []
    replace (st:sts) (poss_st:poss_sts) max_val
      | st <= 0
            = [(st, poss_st)] ++ replace sts poss_sts max_val
      | (st > 0 && st <= max_val)
            = replace sts (poss_st:poss_sts) max_val
      | otherwise
            = [(st, poss_st)] ++ replace sts poss_sts max_val
    -- Now, replace_by has all the inital states along with what they
    -- will be replaced by
    replace_by
        = tidy replace_vals ++ [(x, x) | x <- states,
                                x > 0, x <= max_val]
    -- The normalised start state is found here
    norm_start_state
      =  head [n_s_st | (x, n_s_st) <- replace_by,
                y <- states,
                y == start_state,
                y == x]
    -- The normalised transitions are found here
    norm_delta
      = [((sf, sym), st) | ((x, symbol), y) <- delta,
          (u, sf) <- replace_by,
          (v, st) <- replace_by,
          sym <- alphabet,
          u == x, v == y,
          symbol == sym]
    -- The normal accept statements are found ehre
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

--------------------------------------------------------------------------

--  For a given DFA d, generate a DFA d' so that the languages of d
--  and d' are complementary

complement :: DFA -> DFA
complement (states, alphabet, delta, start_state, accept_states)
  = complete (states', alphabet', delta', start_state', accept_states'')
      where
         -- Find the full DFA which has been normalised and completed
          (states', alphabet', delta', start_state', accept_states')
              = full (states, alphabet, delta, start_state, accept_states)
         -- Complement the accept states of the DFA
          accept_states'' = states' \\ accept_states'

-------------------------------------------------------------------------

--  Given DFAs d1 and d', generate a DFA for the intersection of the
--  languages recognised by d1 and d2.

prod :: DFA -> DFA -> DFA
prod dfa1 dfa2
    =  prod_dfa
    where
        -- First dfa normalised and completed
        (states1, alphabet1, delta1, start_state1, accept_states1)
            =  complete dfa1
        -- Second dfa normalised and completed
        (states2, alphabet2, delta2, start_state2, accept_states2)
            =  complete dfa2
        -- The product dfa
        prod_dfa
            = complete (prod_states, common_alphabet, prod_delta,
                prod_start_state, prod_accept_states)
        -- Maximum of the length of the two dfas
        max_val = maximum [length states1, maximum states1,
                    maximum states2, length states2]
        -- We multiply by the first dfa's state number by max_val to
        -- get a unique value
        prod_states
            = [(x * max_val + y) | x <- states1, y <- states2]
        -- Similar procedure to find the start state
        prod_start_state
            = start_state1 * max_val + start_state2
        -- Similarly for accept states of the product
        prod_accept_states
            = [(x * max_val + y) | x <- accept_states1, y <- accept_states2]
        -- We assume that both the languagages have the same alphabet so the
        -- intersection of the alphabets will be the same as the ablphabets
        -- themselves
        common_alphabet
            = intersect alphabet1 alphabet2
        -- The transition in tuple format for the product DFA
        tuple_delta
            = tidy [(((x, y), sym), (u, v)) |
                ((x, sym), u) <- delta1,
                ((y, s), v) <- delta2,
                s <- common_alphabet,
                sym == s]
        -- The transitons of the product
        prod_delta
            = tidy [((x * max_val + y, sym), u * max_val + v) |
                sym <- common_alphabet,
                ((x, sym), u) <- delta1,
                ((y, sym), v) <- delta2,
                (((a, b), sym), (c, d)) <- tuple_delta,
                (x, y) == (a, b),
                (u, v) == (c, d)]



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
