import Text.ParserCombinators.Parsec

{- A CSV file contains 0 or more lines, each of which is terminated
   by the end-of-line character (eol). -}
-- csvFile :: GenParser Char st [[String]]
-- csvFile =
--   do result <- many line
--      eof
--      return result

-- Each line contains 1 or more cells, separated by a comma
-- line :: GenParser Char st [String]
-- line =
--   do result <- cells
--      eol
--      return result

{- Build up a list of cells. Try to parse the first cell, then
   figure out what ends the cell. -}
