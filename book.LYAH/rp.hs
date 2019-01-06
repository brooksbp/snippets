import Data.List

--represent stack as list... stack 10, 4, 3 = [3,4,10]

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldingFunction [] . words
  where foldingFunction (x:y:ys) "*" = (x*y):ys
        foldingFunction (x:y:ys) "+" = (x+y):ys
        foldingFunction (x:y:ys) "-" = (y-x):ys
        foldingFunction (x:y:ys) "/" = (y/x):ys
        foldingFunction (x:y:ys) "^" = (y**x):ys
        foldingFunction (x:xs) "ln" = log x:ys
        foldingFunction xs "sum" = [sum xs]
        foldingFunction xs numberString = read numberString:xs