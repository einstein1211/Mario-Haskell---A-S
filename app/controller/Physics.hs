module Controller.Physics where

import Model.Model
import Model.Basic
import Model.Platforms
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
    plf obj = playerPhysics gstate $ obj  {plyPhysics = physics' gstate secs (plyPhysics obj)}
    enf obj = obj  {ePhysics   = physics' gstate secs (ePhysics obj)}
    itf obj = obj  {iPhysics   = physics' gstate secs (iPhysics obj)}

physics' :: GameState -> Float -> Physics -> Physics
physics' g s p = checks p {pos = (x',y'), vel = (vx',vy')}
  where
    (x,y)   = pos p
    (vx,vy) = vel p
    (ax,ay) = acc p
    x'  = x   + vx*s
    y'  = y   + vy*s
    vx' = vx  + ax*s
    vy' = vy + (ay+grav)*s
    checks k = colissionCheck $ platformCheck g $ maxSpdCheck $ groundCheck k

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

platformCheck :: GameState -> Physics -> Physics
platformCheck g p = foldr platformCheck' p plats
  where
    plats = platforms g
    platformCheck' ::  Platform -> Physics -> Physics
    platformCheck' plt obj 
      | bo <= tp && bo > bp && ((ro > lp && rp > ro) || (lo < rp && lp < lo))  = obj {gnd = GROUNDED, pos = yup}
      | to > bp && bo < bp && ((ro > lp && rp > ro) || (lo < rp && lp < lo))   = obj {pos = ydown, vel = (vx,-vy), acc = (ax,-ay)}
      -- | (to > bp && bo < bp) || (bo < tp && bo > bp) && ro > lp && rp > ro     = obj {pos = xleft} --bugged
      -- | (bo < tp && bp < bo) && ((ro > lp && rp > ro) || (lo < rp && lp < lo)) = obj {pos = pos'}
      -- | (to > bp && tp > to) && ((ro > lp && rp > ro) || (lo < rp && lp < lo)) = obj {pos = pos'}
      | otherwise = obj 
      where
        (ox,oy) = pos obj
        (vx,vy) = vel obj
        (ax,ay) = acc obj
        yup = (ox,oy+((oh/2)+(ph/2)-abs(oy-py)))
        ydown = (ox,oy-((oh/2)+(ph/2)-abs(py-oy)))
        xleft = (ox-((ow/2)+(pw/2)-abs(ox-px)),oy)
        pos' 
          | (px-ox) > (py-oy) && (px-ox) > (oy-py) = (px-(pw/2)-(ow/2),oy)
          | (ox-px) > (py-oy) && (ox-px) > (oy-py) = (px+(pw/2)+(ow/2),oy)
          -- | (oy-py) > (ox-px) && (oy-py) > (px-ox) = (ox,oy+(ph/2)+(oh/2))
          | otherwise         = (ox,oy-(ph/2)-(oh/2))
        HB ow oh  = (\(HB c d) -> HB (c*scaling) (d*scaling)) (htb obj)
        HB pw ph  = (\(HB c d) -> HB (c*scaling) (d*scaling)) (pltHitbox plt)
        (lo,ro) = (ox-(ow/2),ox+(ow/2))
        (bo,to) = (oy-(oh/2),oy+(oh/2))
        (px,py) = gridPos (pltPos plt)
        (lp,rp) = (px-(pw/2),px+(pw/2))
        (bp,tp) = (py-(ph/2),py+(ph/2))


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