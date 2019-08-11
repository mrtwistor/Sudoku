module Sudoku where

import           Control.Lens
import           Control.Monad
import qualified Data.List     as L

type Board = [[Maybe Int]]

boxSize = 3

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

maybeListToList :: Maybe [a] -> [a]
maybeListToList Nothing  = []
maybeListToList (Just x) = x

rowConstraint :: Board -> Int ->  Bool
rowConstraint board i =
    let row = board!!i >>= maybeToList
    in row == L.nub row

colConstraint :: Board -> Int ->  Bool
colConstraint board i =
    let col = (!!i) <$> board >>= maybeToList
    in col == L.nub col

boxConstraint :: Board -> Int -> Int -> Bool
boxConstraint board i j =
    let box =
          [
            board !! k !! l
          | k <- [boxSize*i..boxSize*(i+1)-1]
          , l <-[boxSize*j..boxSize*(j+1)-1]
          ]
          >>= maybeToList
    in box == L.nub box


allConstraints :: Board -> Bool
allConstraints board =
    let rows = all (rowConstraint board) [0.. -1+boxSize*boxSize]
        cols = all (colConstraint board) [0.. -1+boxSize*boxSize]
        boxes = foldr (&&) True
          $ ( boxConstraint board <$> [0.. -1+boxSize] ) <*> [0.. -1+boxSize]
    in rows && boxes && cols

findNothing b =
  -- Find the next blank space.
  do
    let rows = map  (L.findIndex (==Nothing)) b
    col <- L.findIndex (/=Nothing) rows
    row <- rows!!col
    return (row, col)


extendBoard :: Board -> [Board]
extendBoard b =
  maybeListToList
    $  do
        (row,col) <- findNothing b
        return $ [1.. boxSize*boxSize] >>= getBoards (row,col)
          where
            getBoards (row,col) k = do
              let b' = b & (ix col).(ix row) .~ Just k
              guard $ allConstraints b'
              return b'


solveSudoku :: Board -> [Board]
solveSudoku b
  | isDone b   = [b]
  | otherwise  = extendBoard b >>= solveSudoku
  where isDone = (Nothing ==) . findNothing



justify n | n==0 = Nothing
          | otherwise = Just n

printBoard b =
  foldr (>>) (return()) (print <$> b)

validBoard b =
  all ( (==boxSize*boxSize) . length) b
  && length b == boxSize*boxSize

getBoard :: IO [[Int]]
getBoard = do
  board <- getRows (boxSize*boxSize)
  if not (validBoard board)
    then
         fail "Error in board input, check dimensions."
    else return board
  where
    getRows :: Int -> IO [[Int]]
    getRows 0 = return []
    getRows n = do
      rowString <- getLine
      rows <- getRows (n-1)
      return $ processLine rowString : rows
    processLine :: [Char] -> [Int]
    processLine []    = []
    processLine (h:t) = read [h] : processLine t


main = do
  putStrLn "Enter your board (one row per line, digits only, 0 counts as blank):"
  board <- ((justify <$>) <$>) <$> getBoard
  print "Solution(s):"
  foldr (>>) (return()) (printBoard <$> solveSudoku board)


