import Control.Monad (when)
import Data.Function ((&))
import Data.List (intercalate, transpose)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import System.Process (system)
import System.Random (randomRIO)
import Text.Printf (printf)

type Tile = Int

type Board = [[Tile]]

type Location = (Int, Int)

data Direction = NoOp | L | R | U | D deriving (Eq)

-- Game Constants
goal = 2048 :: Int

tileWidth = 2 + length (show goal)

dimension = 4 :: Int

divideLine :: String
divideLine = replicate ((tileWidth + 1) * dimension + 1) '-'

emptyBoard :: Board
emptyBoard = replicate dimension (replicate dimension 0)

displayTile :: Tile -> String
displayTile 0 = replicate tileWidth ' '
displayTile x = (spaces pre) ++ (show x) ++ (spaces post)
  where
    len = length $ show x
    pre = (tileWidth + 1 - len) `div` 2
    post = tileWidth - pre - len
    spaces n = replicate n ' '

hitGoal :: Board -> Bool
hitGoal = any (any (\x -> x == goal))

setValue :: Board -> (Location, Tile) -> Board
setValue board ((x, y), t) = rowsBefore ++ [updatedRow] ++ rowsAfter
  where
    (rowsBefore, row : rowsAfter) = splitAt x board
    (itemsBefore, _ : itemsAfter) = splitAt y row
    updatedRow = itemsBefore ++ [t] ++ itemsAfter

addTile :: Board -> IO Board
addTile board = do
  if length emptyLoc == 0
    then return board
    else do
      p <- randomRIO (0, 1) :: IO Double
      index <- randomRIO (0, length emptyLoc - 1) :: IO Int
      return $ setValue board (emptyLoc !! index, newVal p)
  where
    enumerate = zip [0 ..]
    emptyLoc =
      [ (i, j) | (i, x) <- enumerate board, (j, y) <- enumerate x, y == 0
      ]
    newVal p = if p < 0.9 then 2 else 4

-- TODO: allow arrow keys
getDirection :: Char -> Direction
getDirection x
  | x == 'h' = L
  | x == 'j' = D
  | x == 'k' = U
  | x == 'l' = R
  | otherwise = NoOp

-- update board
update :: Board -> Direction -> Board
update board NoOp = board
update board L = fmap (fillEmpty . mergeLeft . (filter (\x -> x /= 0))) board
  where
    mergeLeft (x : y : xs) = if x == y then (2 * x) : (mergeLeft xs) else x : (mergeLeft (y : xs))
    mergeLeft xs = xs
    fillEmpty xs = xs ++ (replicate (dimension - (length xs)) 0)
update board R = fmap reverse (update (fmap reverse board) L)
update board U = transpose (update (transpose board) L)
update board D = transpose (update (transpose board) R)

hasMove :: Board -> Bool
hasMove board = any (\d -> update board d /= board) [L, R, U, D]

printBoard :: Board -> IO ()
printBoard board = do
  -- putStr "\ESC[2J"
  system "clear"
  let s = showBoard board
  putStrLn s
  where
    showBoard board =
      (fmap . fmap) displayTile board
        & fmap (intercalate "|")
        & intercalate (printf "|\n%s\n|" divideLine)
        & \s -> (printf "%s\n|%s|\n%s" divideLine s divideLine)

mainLoop :: Board -> IO ()
mainLoop board = do
  printBoard board
  c <- getChar
  let d = getDirection c
  when (d == NoOp) $ mainLoop board
  let updatedBoard = update board d
  if hitGoal updatedBoard
    then do
      printBoard updatedBoard
      putStrLn "You Win!"
    else do
      currentBoard <- addTile updatedBoard
      printBoard currentBoard
      if (hasMove board)
        then mainLoop currentBoard
        else putStrLn "Game Over!"

main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  initBoard <- addTile emptyBoard >>= addTile
  mainLoop initBoard
