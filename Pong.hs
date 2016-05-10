import Simple
import ListFrame

import Data.Maybe
import qualified Network.Socket as Sock

type Player = (Int, Int)
type Ball = [(Float, Float)]
type Direction = (Float, Float)
data State = State Player Player Ball Direction

move :: Int -> (Int, Int) -> String -> (Int, Int)
move _ (p1, p2) "\"A\""    | p2 >= playerWidth = (p1, p2 - 1)
                           | otherwise = (p1, p2)
move ydim (p1, p2) "\"B\"" | p2 < ydim - playerWidth - 1 = (p1, p2 + 1)
                           | otherwise = (p1, p2)
move _ (p1, p2) "\"w\""    | p1 >= playerWidth = (p1 - 1, p2)
                           | otherwise = (p1, p2)
move ydim (p1, p2) "\"s\"" | p1 < ydim - playerWidth - 1 = (p1 + 1, p2)
                           | otherwise = (p1, p2)
move _ (p1, p2) _ = (p1, p2)

colorPixel :: (Int, Int) -> (Int, Int) -> State -> Pixel
colorPixel (xdim, ydim) (x, y) (State (p1, l1) (p2, l2) bs _)
    | y == ydim - 1 && (x < l1 || x > xdim - l2 - 1) = Pixel 0x00 0xFF 0x00
    | y == ydim - 1 = Pixel 0x60 0x60 0x60
    | elem (x,y) $ map (\(x,y) -> (round x, round y)) bs = Pixel 0xFF 0x00 0x00
    | x == 0 && abs (y - p1) < playerWidth = Pixel 0xFF 0xFF 0xFF
    | x == xdim - 1 && abs (y - p2) < playerWidth = Pixel 0xFF 0xFF 0xFF
    | otherwise = Pixel 0x00 0x00 0x00

toFrame :: (Int, Int) -> State -> ListFrame
toFrame (xdim, ydim) state = ListFrame $ map (\y -> map (\x -> colorPixel dim (x,y) state) [0 .. xdim - 1]) [0 .. ydim - 1]

update :: [Event String] -> State -> (ListFrame, State)
update events (State (p1, l1) (p2, l2) ball@((bx, by):_) (dx, dy)) = (toFrame dim state', state')
    where state' | l1' <= 0 || l2' <= 0 = startState dim
                 | otherwise            = State (p1', l1') (p2', l2') ((bx',by'):init ball) (dx',dy')
          (p1', p2') = foldl (\acc (Event mod ev) -> if mod == "KEYBOARD" then move (snd dim) acc ev else acc) (p1, p2) events
          (bx', by') = (bx + dx, by + dy)
          l1' = if round bx' <= 0 then l1 - 1 else l1
          l2' = if round bx' >= fst dim - 1 then l2 - 1 else l2
          dy'    | round by' <= 0 = abs dy
                 | round by' >= snd dim - 2 = -abs dy
                 | round bx' == 1 && abs (by' - fromIntegral p1') < fromIntegral playerWidth = 0.5 * (by' - fromIntegral p1')
                 | round bx' == fst dim - 2 && abs (by' - fromIntegral p2') < fromIntegral playerWidth = 0.5 * (by' - fromIntegral p2') 
                 | otherwise = dy
          dx'    | round bx' == 1 && abs (by' - fromIntegral p1') < fromIntegral playerWidth = sqrt (1 - dy' * dy')
                 | round bx' == fst dim - 2 && abs (by' - fromIntegral p2') < fromIntegral playerWidth = -sqrt (1 - dy' * dy') 
                 | round bx' <= 0 = abs dx
                 | round bx' >= fst dim - 1 = -abs dx
                 | otherwise = dx

startState :: (Int, Int) -> State
startState (x,y) = State (quot y 2 - 1, 3) (quot y 2 - 1, 3) (replicate 2 (1, fromIntegral y / 2 - 1)) (sqrt 0.5, sqrt 0.5)

dim :: (Int, Int)
dim = (30, 12)

playerWidth :: Int
playerWidth = 2

main :: IO ()
main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "127.0.0.1") 1337 dim (Just 80000) True []) update (startState dim)
