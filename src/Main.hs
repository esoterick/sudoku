--
-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons/
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
import Data.Matrix

main :: IO()
main = do putStrLn "rage"
          -- putStrLn $ prettyMatrix $ zero 9 9
          putStrLn $ prettyMatrix $ fromList 9 9 [1..]
          putStrLn $ puzzle 1

puzzle :: Int -> String
puzzle _ = prettyMatrix $ m2
           -- prettyMatrix $ submatrix 1 3 1 3 m

    where m = fromList 9 9 [1..]
          m2 = getSubMatrix (whichSubMatrix (5,5)) m

whichSubMatrix :: (Int, Int) -> (Int, Int)
whichSubMatrix (row, col) = (r, c)
    where r = ((row - 1) `div` 3) + 1
          c = ((col - 1) `div` 3) + 1

getSubMatrix :: (Int, Int) -> Matrix a -> Matrix a
getSubMatrix (row, col) primaryMatrix = subMatrix
    where rowStart = (row * 3) - 2
          rowEnd = row * 3
          colStart = (col * 3) - 2
          colEnd = col * 3
          subMatrix = submatrix rowStart rowEnd colStart colEnd primaryMatrix
