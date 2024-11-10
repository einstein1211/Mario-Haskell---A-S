module Controller.Physics where

import Model.Model
import Model.Basic
import Model.Player
import Model.Enemy
import Model.Block
import Model.Item
import Model.Platform
import Model.Level
import View.Scaling
import Graphics.Gloss.Interface.IO.Game
import Data.Bifunctor

import qualified Data.Map as Map

grav :: Float
grav = -2000

fallspd :: Float
fallspd = -3000

friction :: Float
friction = 0.9

applyPhysics :: Float -> GameState -> GameState
applyPhysics secs gstate =
  gstate {
    players = map plf (players gstate),
    enemies = map enf (enemies gstate),
    items   = map itf (items gstate)
    }
  where
    plf obj = playerPhysics gstate $ obj  {pType = applyPhysics' secs gstate (pType obj), pJumpTime = pJumpTime obj - secs}
    enf obj = obj  {eType = applyPhysics' secs gstate (eType obj)}
    itf obj = obj  {iType = applyPhysics' secs gstate (iType obj)}

applyPhysics' :: Float -> GameState -> Entity -> Entity
applyPhysics' s g e@(MkEntity _ p _) = checks e {physics = p'}
  where
    (x,y)   = pos p
    (vx,vy) = vel p
    (ax,ay) = acc p
    grounded = gnd p == GROUNDED
    x'  = x   + vx*s
    y'  = y   + vy*s
    enttype = entity e
    vx' = if enttype == MkPlayerType MARIO then vxpl' else vx
    vxpl' 
      | grounded && vx<5 && vx>(-5) = 0 + ax*s
      | grounded && vx>0            = vx*friction + ax*s
      | grounded && vx<0            = vx*friction + ax*s
      | otherwise                   = vx + ax*s
    vy'
      | grounded && vy<0  = 0
      | grounded && vy==0 = vy + ay*s
      | otherwise         = vy + (ay+grav)*s
    p'
      | enttype == MkItemType MUSHROOM = p {pos = (x',y'), vel = (vx,vy')}
      | otherwise                      = p {pos = (x',y'), vel = (vx',vy')}
    checks k = maxSpdCheck $ collisionCheck $ platformCheck g $ blockCheck g k

playerPhysics :: GameState -> Player -> Player
playerPhysics g pl = pl {pType = typ',pJumpTime = jmpt',pMovement=movement}
  where
    typ = pType pl
    typ' = typ {physics = phys'}
    keys  = pressedKeys g
    space = KeySpace `elem` keys
    up    = KeyUp `elem` keys
    down  = KeyDown `elem` keys
    left  = KeyLeft `elem` keys
    right = KeyRight `elem` keys
    shft = KeyShiftL `elem` keys
    none = not (space||up||left||right)
    phys  = physics typ
    grounded = gnd phys == GROUNDED
    (x,y) = getPos pl
    (ax,ay) = acc phys
    movl
      | grounded&&left     = -300
      | not grounded&&left = -100
      | otherwise          = 0
    movr
      | grounded&&right     = 300
      | not grounded&&right = 100
      | otherwise           = 0
    jump
      | grounded&&jmpt>0     = (-grav)
      | not grounded&&jmpt>0 = 0.5*(-grav)
      | otherwise            = 0
    phys' = phys {acc = acc',mxv = mv',pos = (x',y), dir = dir'}
    x' = min x xThresHold
    xThresHold = fromIntegral (fst res) * 0.125 * windowScale g
    mv' = if shft then (700,800) else (300,800)
    acc'
      | none = (0,0)
      | jmpt > 0 = (ax+movl+movr,ay+jump)
      | otherwise = (ax+movl+movr,0)
    jmpt = pJumpTime pl
    jmpt'
      | up&&grounded      = 0.2
      | space&&grounded   = 0.2
      | not (up||space)   = 0
      | otherwise         = jmpt
    movement
      | left = WALKING
      | right = WALKING
      | down = CROUCHING
      | otherwise = pMovement pl
    dir'
      | left    = LEFT
      | right   = RIGHT
      | otherwise = dir phys

maxSpdCheck :: Entity -> Entity
maxSpdCheck e@(MkEntity _ p _) = e {physics = p {vel = (vx',vy')}}
  where
    (vx,vy)   = vel p
    (mvx,mvy) = mxv p
    vx' | vx > mvx = mvx
        | vx < (-mvx) = -mvx
        | otherwise = vx
    vy' | vy > mvy = mvy
        | vy < (-mvy) = -mvy
        | otherwise = vy

inHitbox :: Point -> Point -> Hitbox -> Bool
inHitbox (x1,y1) (x2,y2) (MkHB w h) = x1>=lp && y1>=bp && x1<=rp && y1<=tp
  where
    (lp,rp) = (x2-(w/2),x2+(w/2))
    (bp,tp) = (y2-(h/2),y2+(h/2))
--BUG: not bouncing off underside of blocks

intersects :: Point -> Hitbox -> Point -> Hitbox -> Bool
intersects p1@(x1,y1) hb1@(MkHB w1 h1) p2@(x2,y2) hb2@(MkHB w2 h2) =
  inHitbox c1 p2 hb2 || inHitbox c2 p2 hb2 || inHitbox c3 p2 hb2 || inHitbox c4 p2 hb2
  || inHitbox c5 p1 hb1 || inHitbox c6 p1 hb1 || inHitbox c7 p1 hb1 || inHitbox c8 p1 hb1
    where
      c1 = (x1+(w1/2),y1+(h1/2))
      c2 = (x1-(w1/2),y1+(h1/2))
      c3 = (x1+(w1/2),y1-(h1/2))
      c4 = (x1-(w1/2),y1-(h1/2))
      c5 = (x2+(w2/2),y2+(h2/2))
      c6 = (x2-(w2/2),y2+(h2/2))
      c7 = (x2+(w2/2),y2-(h2/2))
      c8 = (x2-(w2/2),y2-(h2/2))

blockCheck :: GameState -> Entity -> Entity
-- blockCheck g e@(MkEntity _ p _) = Map.foldr f e {physics = p {gnd=AIRBORNE}} (slidingWindow g)
blockCheck g e@(MkEntity _ p _) = foldr blockCheck' e {physics = p {gnd=AIRBORNE}} blks
  where
    blks = map (scaleTo (entityScale g)) $ Map.foldr (\c ac -> getEntries c++ac) [] (slidingWindow g)
    blockCheck' :: Block -> Entity -> Entity
    blockCheck' (MkBlock typ plt _ _) (MkEntity _ obj _)
      | intersects opos ohb ppos phb = e {physics=obj'}
      | otherwise = e {physics=obj}
      where
        opos@(ox,oy) = pos obj
        ppos@(px,py) = pfPos plt
        (vx,vy) = vel obj
        (ax,ay) = acc obj
        ohb@(MkHB ow oh) = htb obj
        phb@(MkHB pw ph) = pfHitbox plt
        hidden = typ == HIDDENBLOCK
        obj'
          | hidden                  = obj
          | abs (ox-px)>abs (oy-py) = sides
          | oy < py                 = obj {pos = ydown, vel = (vx,-vy), acc = (ax,0)}
          | otherwise               = obj {gnd = GROUNDED, pos = yup}
        sides
          | ox < px = obj   {pos = xleft, vel = (0,vy), acc = (0,ay)}
          | otherwise = obj {pos = xright, vel = (0,vy), acc = (0,ay)}
        yup   = (ox,oy+((oh/2)+(ph/2)-abs (oy-py))-1)
        ydown = (ox,oy-((oh/2)+(ph/2)-abs (py-oy))+1)
        xleft = (ox-((ow/2)+(pw/2)-abs (ox-px))-1,oy)
        xright= (ox+((ow/2)+(pw/2)-abs (ox-px))+1,oy)

platformCheck :: GameState -> Entity -> Entity
platformCheck g e = foldr platformCheck' e plats
  where
    plats = map (scaleTo (entityScale g)) $ Map.foldr (\c ac -> getEntries c++ac) [] (slidingWindow g)
    platformCheck' ::  Platform -> Entity -> Entity
    platformCheck' plt (MkEntity _ obj _)
      | intersects opos ohb ppos phb = e {physics=obj'}
      | otherwise = e {physics=obj}
      where
        opos@(ox,oy) = pos obj
        ppos@(px,py) = pfPos plt
        (vx,vy) = vel obj
        (ax,ay) = acc obj
        ohb@(MkHB ow oh) = htb obj
        phb@(MkHB pw ph) = pfHitbox plt
        obj'
          | abs (ox-px)>abs (oy-py) = sides
          | oy < py               = obj {pos = ydown, vel = (vx,-vy), acc = (ax,0)}
          | otherwise             = obj {gnd = GROUNDED, pos = yup}
        sides
          | ox < px = obj   {pos = xleft, vel = (0,vy), acc = (0,ay)}
          | otherwise = obj {pos = xright, vel = (0,vy), acc = (0,ay)}
        yup   = (ox,oy+((oh/2)+(ph/2)-abs (oy-py))-1)
        ydown = (ox,oy-((oh/2)+(ph/2)-abs (py-oy))+1)
        xleft = (ox-((ow/2)+(pw/2)-abs (ox-px))-1,oy)
        xright= (ox+((ow/2)+(pw/2)-abs (ox-px))+1,oy)

collisionCheck :: Entity -> Entity --TODO: SUPER BAD FUNCTION GARBAGE PLS FIXXXXXX MEEEEE
collisionCheck e@(MkEntity _ p _) 
  | killboundary = kill e 
  | otherwise = e {physics = p {pos = (x',y), vel = (vx',vy), acc = (ax',ay)}}
  where
    (x,y)   = pos p
    (vx,vy) = vel p
    (ax,ay) = acc p
    g       = gnd p
    w = (\(MkHB c d) -> c) (htb p)
    (l,r) = (x-(w/2),x+(w/2))
    (vx',ax') = (vx,ax)-- if r > fst uppbound || l < fst lowbound then (0,0) else (vx,ax)
    killboundary = x < (-((fromIntegral (fst res))*0.5) - 100) || y < (-((fromIntegral (snd res))*0.5))
    x' = x
      -- | r > fst uppbound = x-1
      -- | l < fst lowbound = x+1
      -- | otherwise = x

-- lowbound = (fromIntegral (-fst res) / 2, fromIntegral (-snd res) / 2)