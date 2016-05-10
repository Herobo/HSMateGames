import Simple
import ListFrame

import Data.Maybe
import Data.Bits
import qualified Network.Socket as Sock

type Player = [(Int,Int)]
type Obstacles = [(Int,Int)]
type Points = Int
type Seed = Int

data State = State Player Player Obstacles Points Seed

newObstacle :: (Int, Int) -> State -> Obstacles
newObstacle (xdim, ydim) (State player _ obstacles points seed) = [(x, y `mod` (ydim-1)) | x <- [xdim..xdim + size], y <- [pos..pos + size]]
    --weird computations for pure randomness
    where pos = (foldl (\acc (x, y) -> xor (acc + x) y) seed (obstacles ++ player)) `mod` (ydim-1)
          size = 1 + (foldl (\acc (x, y) -> xor (acc + y) x) seed (obstacles ++ player)) `mod` 2

move :: (Int, Int) -> Player -> String -> Player
move (xdim,ydim) player "\"A\"" = map (\(x, y) -> (x, (y - 1) `mod` (ydim - 1))) player
move (xdim,ydim) player "\"B\"" = map (\(x, y) -> (x, (y + 1) `mod` (ydim - 1))) player
move (xdim,ydim) player "\"D\"" | any (\(x, _) -> x == 0) player = player
                                | otherwise = map (\(x, y) -> ((x - 1) `mod` xdim, y)) player
move (xdim,ydim) player "\"C\"" | any (\(x, _) -> x == xdim - 1) player = player
                                | otherwise = map (\(x, y) -> ((x + 1) `mod` xdim, y)) player
move _ player _ = player

colorPixel :: (Int, Int) -> (Int, Int) -> State -> Pixel
colorPixel (xdim, ydim) pixel@(x, y) (State player _ obstacles p _)
    | y == ydim - 1 && p .&. 2^(xdim - x - 1) > 0 = Pixel 0x00 0xff 0x00
    | y == ydim - 1 = Pixel 0x60 0x60 0x60
    | elem pixel player = Pixel 0xff 0xff 0xff
    | elem pixel obstacles = Pixel 0xff 0x00 0x00
    | otherwise = Pixel 0x00 0x00 0x00

toFrame :: (Int, Int) -> State -> ListFrame
toFrame (xdim, ydim) state = ListFrame $ map (\y -> map (\x -> colorPixel dim (x,y) state) [0 .. xdim - 1]) [0 .. ydim - 1]

update :: [Event String] -> State -> (ListFrame, State)
update events state@(State player remove obstacles points seed) = (toFrame dim state', state')
    where state' | any (\x -> elem x obstacles) player = startState seed
                 | otherwise = State player' remove' obstacles' points' (seed + 1)
          player' = foldl (\acc (Event mod ev) -> if mod == "KEYBOARD" then move dim acc ev else acc) player events
          obstacles' = filter (\x -> not $ elem x remove) (newObstacle dim state ++ oldObstacles)
          oldObstacles = filter (\(x, y) -> x >= 0) $ map (\(x, y) -> (x - 1, y)) obstacles
          points' = points + length obstacles - length oldObstacles
          --weird computations to generate path
          remove' = map (\(x, y) -> (x, (y + random) `mod` (snd dim - 1))) remove
          random  = (foldl (\acc (x, y) -> xor (acc + x) y) 0 oldObstacles) `mod` 3 - 1

startState :: Seed -> State
startState seed = State [(0,0),(0,1),(1,0),(1,1)] [(x,y) | x <- [fst dim..fst dim+3], y <- [0..3]] [] 0 seed

dim :: (Int, Int)
dim = (30, 12)

main :: IO ()
main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "127.0.0.1") 1337 dim (Just 125000) True []) update (startState 314159265)
