module MateGames (
  Event(..)
  ,stringEvent
  ,runMate
  ,parseAddress
  ,Config(..)
  ,Frame(..)
  ,Game(..)
) where

import Simple
import ListFrame

data Game a = Running a | Dead ListFrame Int
