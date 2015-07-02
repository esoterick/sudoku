--
-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons/
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
import System.Environment
import Data.Matrix

main = do putStrLn "rage"
          putStrLn $ prettyMatrix $ zero 9 9
          putStrLn $ puzzle 1

data Block = Int | Nothing

puzzle :: Int -> String
puzzle _ = prettyMatrix $ submatrix 1 3 1 3 m
    where m = fromList 9 9 [1..]

whichSubMatrix :: (Int, Int) -> (Int, Int)
whichSubMatrix (row, col) = (r, c)
    where r = ((row - 1) `div` 3) + 1
          c = ((col - 1) `div` 3) + 1

getSubMatrix :: (Int, Int) -> Matrix a -> Matrix a
getSubMatrix (row, col) primaryMatrix = subMatrix
    where rowStart = (row * 3) - 3
          rowEnd = row * 3
          colStart = (col * 3) - 3
          colEnd = col * 3
          subMatrix = submatrix rowStart rowEnd colStart colEnd primaryMatrix
