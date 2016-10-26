module Snake (
  State(..)
  ,update
  ,startState
) where

import MateGames
import ListFrame

import Data.Maybe
import Data.Bits
import qualified Network.Socket as Sock

data State = State [(Int, Int)] (Int, Int) Points Seed Direction

type Seed = Int
type Points = Int

data Direction = L | R | U | D

newDirection :: Direction -> Direction -> String -> Direction
newDirection U d "\"B\"" = d
newDirection _ _ "\"B\"" = D
newDirection D d "\"A\"" = d
newDirection _ _ "\"A\"" = U
newDirection R d "\"D\"" = d
newDirection _ _ "\"D\"" = L
newDirection L d "\"C\"" = d
newDirection _ _ "\"C\"" = R
newDirection _ d _ = d

move :: (Int, Int) -> (Int,Int) -> Direction -> (Int, Int)
move (xdim, ydim) (x, y) D = (x, (y + 1) `mod` (ydim - 1))
move (xdim, ydim) (x, y) U = (x, (y - 1) `mod` (ydim - 1))
move (xdim, ydim) (x, y) L = ((x - 1) `mod` xdim, y)
move (xdim, ydim) (x, y) R = ((x + 1) `mod` xdim, y)

generateTarget :: [(Int, Int)] -> (Int, Int) -> Int -> (Int, Int)
generateTarget snake (xdim, ydim) seed
                  | elem (x, y) snake = generateTarget snake dim (seed + 1)
                  | otherwise = (x, y)
    --weird computations for pure randomness
    where x = (foldl (\acc (x, y) -> xor (acc + y) x) seed snake) `mod` xdim
          y = (foldl (\acc (x, y) -> xor (acc + x) y) seed snake) `mod` (ydim - 1)

colorPixel :: (Int, Int) -> (Int, Int) -> State -> Pixel
colorPixel (xdim, ydim) pixel@(x, y) (State snake target p _ _) 
                                            | y == ydim - 1 && p .&. 2^(xdim - x - 1) > 0 = Pixel 0x00 0xff 0x00
                                            | y == ydim - 1 = Pixel 0x60 0x60 0x60
                                            | pixel == head snake = Pixel 0x00 0xff 0xff
                                            | elem pixel snake = Pixel 0xff 0xff 0xff
                                            | pixel == target = Pixel 0xff 0x00 0x00
                                            | otherwise = Pixel 0x00 0x00 0x00

toFrame :: (Int, Int) -> Game State -> ListFrame
toFrame (xdim, ydim) (Running state) = ListFrame $ map (\y -> map (\x -> colorPixel (xdim, ydim) (x, y) state) [0 .. xdim - 1]) [0 .. ydim - 1]
toFrame (_,_) (Dead f _) = f

update :: [Event String] -> Game State -> (ListFrame, Game State)
update events (Dead f a) = (f, startState a)
update events (Running (State snake@(x:xs) target points seed dir)) = (toFrame dim state', state')
    where state'  | elem x xs = Dead gameOver seed
                  | otherwise = Running (State snake' target' points' (seed + 1) dir')
          points' = if x == target then points + 1 else points
          dir'    = foldl (\acc (Event mod ev) -> if mod == "KEYBOARD" then newDirection dir acc ev else acc) dir events
          snake'  | x == target = move dim x dir : snake
                  | otherwise   = move dim x dir : init snake
          target' | x == target = generateTarget snake' dim seed
                  | otherwise   = target

gameOver :: ListFrame
gameOver = ListFrame [[Pixel 0xff 0x00 0x00 | x <- [1..fst dim]] | y <- [1..snd dim]]

startState :: Int -> Game State
startState seed = Running (State startSnake (generateTarget startSnake dim seed) 0 seed R)

startSnake :: [(Int,Int)]
startSnake = [(2,0), (1,0),(0,0)]

dim :: (Int, Int)
dim = (30, 12)

main :: IO ()
main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "127.0.0.1") 1337 dim (Just 100000) True []) update (startState 0)
