import MateGames
import ListFrame

import qualified Snake as Sn
import qualified Obstacles as Ob
import qualified Pong as Po

import Data.Maybe
import Data.Bits
import qualified Network.Socket as Sock

data State = SnakeState (Game Sn.State) | ObstacleState (Game Ob.State) | PongState (Game Po.State)

newState :: [Event String] -> ListFrame -> Int -> State
newState [] f a = SnakeState (Dead f (a+1))
newState ((Event mod ev):xs) f a | mod == "KEYBOARD" && ev == "\"s\"" = SnakeState (Sn.startState a)
                                 | mod == "KEYBOARD" && ev == "\"o\"" = ObstacleState (Ob.startState a)
                                 | mod == "KEYBOARD" && ev == "\"p\"" = PongState (Po.startState dim)
                                 | otherwise = newState xs f a

update :: [Event String] -> State -> (ListFrame, State)
update events (SnakeState (Dead f a)) = (f, newState events f a)
update events (ObstacleState (Dead f a)) = (f, newState events f a)
update events (PongState (Dead f a)) = (f, newState events f a)
update events (SnakeState state) = let (f, state') = Sn.update events state in (f, SnakeState state')
update events (ObstacleState state) = let (f, state') = Ob.update events state in (f, ObstacleState state')
update events (PongState state) = let (f, state') = Po.update events state in (f, PongState state')

startState = SnakeState (Dead startScreen 0)
startScreen = ListFrame [[Pixel 0xff 0x00 0x00 | x <- [1..fst dim]] | y <- [1..snd dim]]

dim :: (Int, Int)
dim = (30, 12)

main :: IO ()
main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "127.0.0.1") 1337 dim (Just 100000) True []) update startState
