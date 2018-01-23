module SnakeGame where

import Data.Matrix as M
import SnakeBrain
import SnakeUtils
import Control.Monad.Random as R

--------------------
-- | Game utils | --
--------------------
width = 20
height = 20
maxMoves = 50
startDir = (1,0) -- Starts moving to the right

data GameState = GameState {
  snake :: [(Int,Int)],
  points :: Float,
  foodPos :: (Int,Int)
} deriving( Show )
---------------------------

-- | Initiates a random gamestart
initGame :: IO GameState
initGame = do
  snakeHeadX <- randomRIO (2,16)
  snakeHeadY <- randomRIO (2,16)
  let snakeBody = (snakeHeadX-1,snakeHeadY)
  foodX <- randomRIO (5,18)
  foodY <- randomRIO (5,18)
  return $ GameState [(snakeHeadX,snakeHeadY),snakeBody] 100.0 (foodX,foodY)

-- | Takes a snake net and returns the points collected during play
startGame :: SnakeNet -> IO Float
startGame net = do
  print "Starting a new game"
  initialState <- initGame
  runGame net initialState startDir maxMoves

runGame :: SnakeNet -> GameState -> (Int,Int) -> Int -> IO Float
runGame _ (GameState _ p _) _ 0 = return p
runGame net (GameState s p f) dir m = do
  let b = matrix width height (\_ -> 0)
  let board = foldr (\i -> \m -> setElem (-1) i m) (setElem 1 f b) s
  let input = M.fromList 1 (width*height) $ M.toList board -- NOTE: hack
  newDir <- calculate input net
  let (x,y) = nextPos (head s) (updateDirection newDir dir)
  if x <= 0 || y <= 0 || x >= width || y >= height || (board ! (x,y)) == (-1)
  then return p -- Game over
  else
    let val = board ! (x,y) in
    if val == 1
    then return 200.0 -- TODO RANDOMIZE NEW FOOD
    else runGame net (GameState ((x,y):(init s)) (p*0.8) f) (updateDirection newDir dir) (m-1)-- DECAY TO PROMOTE SPEED
      where nextPos (cX,cY) (dX,dY) = (cX+dX,cY+dY)

updateDirection :: Direction -> (Int,Int) -> (Int,Int)
updateDirection d (x,y)
  | d == LEFT  = (-y,x)
  | d == RIGHT = (y,-x)
  | otherwise  = (x,y)
