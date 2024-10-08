module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameState -> IO GameState
step = return physics

physics :: Float -> GameState -> GameState
physics secs gstate = gstate {object = Object {name = RECTANGLE, location = (x',y'),shape = path, vel = Vel vx vy}}
  where
    (x,y) = location (object gstate)
    Vel vx vy = vel (object gstate)
    x' = x + vx*secs
    y' = y + vy*secs
    path = shape (object gstate)

input :: Event -> GameState -> IO GameState
input = undefined

inputKey :: Event -> GameState -> GameState
inputKey = undefined