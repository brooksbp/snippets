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

     
   ^ Wikipedia: F-algebra
     F-algebra is a structure defined according to a functor F. F-algebras can
     be used to represent datastructures such as lists and trees.

     An F-algebra for an endofunctor
              F : C --> C
     is an object A of C together with a C-morphism
              \alpha : F A --> A
     A homomorphism from F-algebra (A,\alpha) to F-algebra (B,\beta) is a
     morphism
              f : A --> B
     in C such that
              f o \alpha = \beta o F f


                         \alpha
                 F(A)  ---------->  A

                  |                 |
                  |                 |
             F(f) |                 | f
                  |                 |
                  !                 !

                 F(B)  ---------->  B
                         \beta

     Example - Consider the functor F : Set --> Set which takes a set x
     and returns x+1. The set N of natural numbers together with the
     function [zero,succ] : 1 + N --> N (which is the coproduct of
     functions zero : 1 --> N, succ : N --> N (which sends integer n
     to n+1)) is an F-algebra.

   What is Structural Recursion?
   In Coq or Agda a programmer enters a recursive definition and the system
   validates whether it is an acceptable definition. An alternative is to
   have the system proclaim acceptable definitions; use special-purpose
   recursion combinators/schemes to write programs. Author interested in
   dependent recursion schemes. First, the most basic recursion scheme...

   Folds, Catamorphisms and F-Algebras
   We present structural recursion on lists (
        foldr : (B : Set) -> B -> (A -> B -> B) -> List -> B
   which satisfies
        foldr B n c nil         = n
        foldr B n c (cons a as) = c a (foldr B c n as)
   The well-known category-theoretic generalisation of this uses F-algebras.
   Constructors of our inductive type are encoded as a functor
        F : Set -> Set
   Constructors of lists with elements from some set A
        ListF : Set -> Set
        ListF X = 1 + A x X
   foldr B n can now be captured as
        f : ListF B -> B
   A pair of set B and such a function f : F B -> B is an F-algebra
        (B,f)
   Given two F-algebras
        k  : F B -> B
        k' : F C -> C
   define morphisms to be functions
        h : B -> C
   such that
        h o k = k' o F h
   F-algebras and their morphisms form the F-Alg category. For most functors F,
   F-Alg has an initial object (unique up-to isomorphism). Consider F-algebra
        in : F G -> G
   such that for any other F-algebra
        k : F S -> S
   there is a unique F-algebra morphism from -in- to -k-. For the list example,
   an initial algebra for ListF is the set of lists with elements from the
   set A, with
        in : ListF (List A) -> List A)
   defined as the construction of lists using nil and cons. If we have a
   ListF-algebra
        k : ListF S -> S
   we get a function of type
        List A -> B
   Since it is a ListF-morphism it satisfies the two equations of foldr. Note
        F X = (X -> 2) -> 2
   does not have an initial algebra in the category of sets and functions. In
   recursion schemes literature, folds are referred to as catamorphisms.

   !!! Edward Kmett
   http://knol.google.com/k/catamorphisms
   http://comonad.com/reader/2009/recursion-schemes/

   What is Structural Induction?
   Structural induction is structural recursion with fancier types.
   Dependent Structural Recursion (aka Structural Inductions)
   In pure type theory (without pattern matching and recursive definitions),
   structural recursion is presented in terms of elimination principles.