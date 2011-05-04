{- On Structural Recursion

   http://personal.cis.strath.ac.uk/~raa/posts/2011-04-22-structural-recursion.html -}

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

{- 1. always terminating. recursive calls always decreases size of inductively
      defined types. need totality of defined functions to retain decidability
      of type checking.
   2. obvious way of computing. what it means to run a function 

   Decreasing size to retain decidability is restrictive, -}

quicksort :: [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort l ++ quicksort (x:h)
  where (l,h) = partition x xs

{- partition will return l,h that should be shorter than x:xs to ensure
   termination. Naive structural recursion cannot see this.

   General Recursion
   Use co-inductive types. Forget termination. Popular approach.

   Termination Checkers
   Let type checker rely on another termination checker. Approach taken by Agda
   2 and Coq. Advantage of natural looking function definitions. Downside of
   divide between type checker and termination checker; types no longer a
   "checkable language of evidence."

   Structural Recursion on Accessibility Predicates
   Use general purpose inductively defined type Acc which encodes recursion on
   arbitrary well-founded relations (Agda syntax): -}

-- data Acc (x : A) : Set where
--  acc : (\/ y -> y < x -> Acc y

{- Functions are defined by structural recursion on Acc x argument. Must provide
   proof that Acc x for chosen relation < and argument x.

   Edward Z. Yang - Well-founded recursion in Agda
   http://blog.ezyang.com/2010/06/well-founded-recursion-in-agda/

   Bengt Nordstrom - Terminating General Recursion

   Probably don't need to generate code for Acc x, maybe just x. Gap between
   semantics of compile-time functions and run-time functions.

   Bove-Capretta method
   http://www.cs.nott.ac.uk/~vxc/publications/General_Recursion_MSCS_2005.pdf
   Generate an inductively-defined predicate for each recursive function.

   New Technique
   Build more complex structural recursion principles from basic structural
   recursion on inductively defined datatypes - so that the data still drives
   the computation. Idea trails back to Eduardo Gimenez justifying Coq's
   termination checker. McBride and McKinna used idea in defining functions
   in type theory via eliminators. Hermida and Jacobs [1] original paper, Neil
   Ghani, Patricia Johann, Clement Fumex [2] generalisation of it present
   structural recursion in category-theory.

   [1] http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.36.7400
   [2] http://personal.cis.strath.ac.uk/~patricia/csl2010.pdf

   ^ Next post on encoding structural recusion in type and category theory. -}

{- On Structural Recurssion II: Folds and Induction

   http://personal.cis.strath.ac.uk/~raa/posts/2011-04-28-folds-and-induction.html

   Goal is create a self-contained, syntax-free definition of a dependently-typed
   structural recursion principle for a type A. Start with normal structural
   recursion over inductively defined types; later move to other types by
   combining existing structural induction principles.

   ^ reddit comments: only covering structural recursion/induction over
     inductively defined types--types that arise as initial fixpoints of
     F-algebra--not co-inductive types.