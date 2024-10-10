module Controller where

import Model
-- import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

grav :: Float
grav = -100.0

fallspd :: Float
fallspd = -2000

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
    physhelp s p = checks p {pos = (x',y'), vel = (vx',vy')}
      where
        (x,y)   = pos p
        (vx,vy) = vel p
        (ax,ay) = acc p
        x'  = x   + vx*s
        y'  = y   + vy*s
        vx' = vx  + ax*s
        vy' = if vy < fallspd && ay < 0 then vy else vy + ay*s
        checks k = colissionCheck $ gravity k
    gravity :: Physics -> Physics
    gravity p = p {acc = (ax,grav)}
      where (ax,_) = acc p
    colissionCheck :: Physics -> Physics
    colissionCheck p = p {vel = (vx',vy'), acc = (ax',ay')}
      where
        (x,y)   = pos p
        (vx,vy) = vel p
        (ax,ay) = acc p
        vx' = if x > fst uppbound || x < fst lowbound then -vx else vx
        vy' = if y > snd uppbound || y < snd lowbound then 0 else vy
        ax' = if x > fst uppbound || x < fst lowbound then -ax else ax
        ay' = if y > snd uppbound || y < snd lowbound then -ay else ay

-- | Handle user input
input :: Event -> GameState -> IO GameState
-- input e gstate = return (inputKey e gstate)
input e = return

-- inputKey :: Event -> GameState -> GameState
-- inputKey (EventKey (Char c) _ _ _) gstate
--   = -- If the user presses a character key, show that one
--     -- gstate { infoToShow = ShowAChar c }
--     gstate
-- inputKey _ gstate = gstate -- Otherwise keep the same