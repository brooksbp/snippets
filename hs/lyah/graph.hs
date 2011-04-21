-- heathrow to london

-- Two roads, A & B, that have crossroads.  Edges weighted.

--        40     9
-- A ------------------
--     |2     |20   |4
-- B ------------------
--        12    90

--input: x,y,z... where x = A weight,
--                      y = B weight,
--                      z = crossroad weight

data Section = Section { getA :: Int
                       , getB :: Int
                       , getC :: Int
                       } deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30
                   ,Section 5 90 20
                   ,Section 40 2 25
                   ,Section 10 8 0]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let priceA = sum $ map snd pathA
      priceB = sum $ map snd pathB
      forwardPriceToA = priceA + a
      