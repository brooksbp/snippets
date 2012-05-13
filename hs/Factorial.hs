module Factorial where

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

-- http://www.haskell.org/ghc/docs/latest/html/users_guide/options-debugging.html

-- ghc -c Factorial.hs -ddump-ds
-- ghc -c Factorial.hs -ddump-simpl
-- ghc -c Factorial.hs -ddump-stg
-- ghc -c Factorial.hs -ddump-cmm
-- ghc -c Factorial.hs -ddump-opt-cmm
-- ghc -c Factorial.hs -ddump-asm

