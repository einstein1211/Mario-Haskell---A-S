module Controller.Physics where

import Model

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
    plf obj = obj  {plyPhysics = physics' secs (plyPhysics obj)}
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
    vy' = if vy < fallspd && ay < 0 then vy else vy + (ay+grav)*s 
    checks k = colissionCheck $ groundCheck k
--Bad max speed implementation
--TODO: make dedicated function

groundCheck :: Physics -> Physics
groundCheck p = p {gnd = groundstate}
  where
    (_,y) = pos p
    HB _ h  = htb p
    b = y-(h/2)
    groundstate = if b < snd lowbound then GROUNDED else AIRBORNE 
--Currently only checks for bottom of screen 
--TODO: implement on all tops of platforms




colissionCheck :: Physics -> Physics --TODO: SUPER BAD FUNCTION GARBAGE PLS FIXXXXXX MEEEEE
colissionCheck p = p {pos = (x',y'), vel = (vx',vy'), acc = (ax',ay')}
  where
    (x,y)   = pos p
    (vx,vy) = vel p
    (ax,ay) = acc p
    g       = gnd p
    HB w h  = htb p
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
    (vx',ax') =
      case g of
        GROUNDED -> if r > fst uppbound || l < fst lowbound then (-vx,-ax) else if vx > 0 then (vx,-friction) else if vx == 0 then (vx,0) else (vx,friction)
        _        -> if r > fst uppbound || l < fst lowbound then (-vx,-ax) else (vx,ax)
    (vy',ay') =
      case g of
        GROUNDED -> if vy < 0 then (0,0) else (vy,ay)
        _        -> (vy,ay)