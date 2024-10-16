module Controller.Physics where

import Model.Model
import Graphics.Gloss.Interface.IO.Game

grav :: Float
grav = -1000.0

fallspd :: Float
fallspd = -3000

friction :: Float
friction = 2000

physics :: Float -> GameState -> GameState
physics secs gstate =
  gstate {
    players = map plf (players gstate),
    enemies = map enf (enemies gstate),
    items   = map itf (items gstate)
    }
  where
    plf obj = playerPhysics gstate $ obj  {plyPhysics = physics' secs (plyPhysics obj)}
    enf obj = obj  {ePhysics   = physics' secs (ePhysics obj)}
    itf obj = obj  {iPhysics   = physics' secs (iPhysics obj)}

physics' :: Float -> Physics -> Physics
physics' s p = checks p {pos = (x',y'), vel = (vx',vy')}
  where
    (x,y)   = pos p
    (vx,vy) = vel p
    (ax,ay) = acc p
    x'  = x   + vx*s
    y'  = y   + vy*s
    vx' = vx  + ax*s
    vy' = vy + (ay+grav)*s
    checks k = colissionCheck $ maxSpdCheck $ groundCheck k

playerPhysics :: GameState -> Player -> Player
playerPhysics g pl = pl {plyPhysics = phys'}
  where
    keys  = pressedKeys g
    space = KeySpace `elem` keys
    up    = KeyUp `elem` keys
    down  = KeyDown `elem` keys
    left  = KeyLeft `elem` keys
    right = KeyRight `elem` keys
    shft = KeyShiftL `elem` keys
    phys  = plyPhysics pl
    (ax,ay) = acc phys
    phys' = phys {acc = acc',mxv = mv'}
    mv'  = if shft then (1000,700) else (500,500)
    acc'
      | up    = (ax,ay+1000)
      | space = (ax,ay+1000)
      | down  = (ax,ay) --TODO: See if the down key has a purpose
      | left  = (ax-300,ay)
      | right = (ax+300,ay)
      | otherwise = (0,0)

maxSpdCheck :: Physics -> Physics
maxSpdCheck p = p {vel = (vx',vy')}
  where
    (vx,vy)   = vel p
    (mvx,mvy) = mxv p
    vx' | vx > mvx = mvx
        | vx < (-mvx) = (-mvx)
        | otherwise = vx
    vy' | vy > mvy = mvy
        | vy < (-mvy) = (-mvy)
        | otherwise = vy

groundCheck :: Physics -> Physics
groundCheck p = p {gnd = groundstate}
  where
    (_,y) = pos p
    HB _ h  = (\(HB c d) -> HB (c*scaling) (d*scaling)) (htb p)
    b = y-(h/2)
    groundstate = if b-1 < snd lowbound then GROUNDED else AIRBORNE --FIXME: Dit is garbage, fix later
--Currently only checks for bottom of screen 
--TODO: implement on all tops of platforms




colissionCheck :: Physics -> Physics --TODO: SUPER BAD FUNCTION GARBAGE PLS FIXXXXXX MEEEEE
colissionCheck p = p {pos = (x',y'), vel = (vx',vy'), acc = (ax',ay')}
  where
    (x,y)   = pos p
    (vx,vy) = vel p
    (ax,ay) = acc p
    g       = gnd p
    HB w h  = (\(HB c d) -> HB (c*scaling) (d*scaling)) (htb p)
    (l,r) = (x-(w/2),x+(w/2))
    (b,t) = (y-(h/2),y+(h/2))
    x'
      | r > fst uppbound = x-(r - fst uppbound)
      | l < fst lowbound = x+(fst lowbound - l)
      | otherwise = x
    y'
      | t > snd uppbound = y-(t - snd uppbound)
      | b < snd lowbound = y+(snd lowbound - b)
      | otherwise = y
    (vx',ax') = if r > fst uppbound || l < fst lowbound then (-vx,-ax) else (vx,ax)
      -- case g of
      --   GROUNDED -> if r > fst uppbound || l < fst lowbound then (-vx,-ax) else if vx > 0 then (vx,ax-friction) else if vx == 0 then (vx,0) else (vx,ax+friction)
      --   AIRBORNE -> if r > fst uppbound || l < fst lowbound then (-vx,-ax) else (vx,ax)
    (vy',ay') =
      case g of
        GROUNDED -> if vy < 0 then (0,0) else (vy,ay)
        AIRBORNE  -> (vy,ay)