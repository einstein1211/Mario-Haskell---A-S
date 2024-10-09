module Controller where

import Model
-- import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameState -> IO GameState
step secs gstate = 
  return $ physics secs gstate

physics :: Float -> GameState -> GameState
physics secs gstate = 
  gstate {
    players = map plf (players gstate),
    enemies = map enf (enemies gstate),
    items   = map itf (items gstate)
    }
  where
    plf obj = obj  {plyPhysics = physhelp secs (plyPhysics obj)}
    enf obj = obj  {ePhysics   = physhelp secs (ePhysics obj)}
    itf obj = obj  {iPhysics   = physhelp secs (iPhysics obj)}
    physhelp :: Float -> Physics -> Physics
    physhelp s p = colissionCheck p {pos = (x',y'), vel = (vx',vy'), acc = (ax,ay)}
      where
        (x,y)   = pos p
        (vx,vy) = vel p
        (ax,ay) = acc p
        x'  = x   + vx*s
        y'  = y   + vy*s
        vx' = vx  + ax*s
        vy' = vy  + ay*s
    colissionCheck :: Physics -> Physics
    colissionCheck p = p {pos = pos p, vel = (vx',vy'), acc = (ax',ay')}
      where
        (x,y)   = pos p
        (vx,vy) = vel p
        (ax,ay) = acc p
        vx' = if x > 640 || x < (-640) then -vx else vx
        vy' = if y > 360 || y < (-360) then -vy else vy
        ax' = if x > 640 || x < (-640) then -ax else ax
        ay' = if y > 360 || y < (-360) then -ay else ay

-- | Handle user input
input :: Event -> GameState -> IO GameState
-- input e gstate = return (inputKey e gstate)
input e = return

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
  = -- If the user presses a character key, show that one
    -- gstate { infoToShow = ShowAChar c }
    gstate
inputKey _ gstate = gstate -- Otherwise keep the same