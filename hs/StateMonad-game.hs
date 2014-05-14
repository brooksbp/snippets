--module StateGame where

import Control.Monad.State

type GameValue = Int
type GameState = (Bool, Int)

playGame :: String -> State GameState GameValue
playGame [] = do
  (_, score) <- get
  return score
playGame (x:xs) = do
  (on, score) <- get
  case x of
    'a' | on -> put (on, score + 1)
    'b' | on -> put (on, score - 1)
    'c'      -> put (not on, score)
    _        -> put (on, score)
  playGame xs

startState :: GameState
startState = (False, 0)

main = print $ evalState (playGame "abcccaabbabacabacbaa") startState
