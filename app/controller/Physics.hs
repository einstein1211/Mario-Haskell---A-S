module Controller.Physics where

import Model.Model
import Model.Basic
import Model.Player
import Model.Enemy
import Model.Block
import Model.Item
import Model.Platform
import Graphics.Gloss.Interface.IO.Game

grav :: Float
grav = -2000

fallspd :: Float
fallspd = -3000

friction :: Float
friction = 2000

applyPhysics :: Float -> GameState -> GameState
applyPhysics secs gstate =
  gstate {
    players = map plf (players gstate),
    enemies = map enf (enemies gstate),
    items   = map itf (items gstate)
    }
  where
    plf obj = playerPhysics gstate $ obj  {pType = applyPhysics' gstate secs (pType obj), pJumpTime = pJumpTime obj - secs}
    enf obj = obj  {eType = applyPhysics' gstate secs (eType obj)}
    itf obj = obj  {iType = applyPhysics' gstate secs (iType obj)}

applyPhysics' :: GameState -> Float -> Entity -> Entity
applyPhysics' g s e@(MkEntity _ p _) = checks e {physics = p {pos = (x',y'), vel = (vx',vy')}}
  where
    (x,y)   = pos p
    (vx,vy) = vel p
    (ax,ay) = acc p
    grounded = gnd p == GROUNDED
    x'  = x   + vx*s
    y'  = y   + vy*s
    vx' = vx  + ax*s
    vy'
      | grounded && vy<0  = 0
      | otherwise         = vy + (ay+grav)*s
    checks k = maxSpdCheck $ collisionCheck $ platformCheck g $ blockCheck g k

playerPhysics :: GameState -> Player -> Player
playerPhysics g pl = pl {pType = typ',pJumpTime = jmpt'}
  where
    typ = pType pl
    typ' = typ {physics = phys'}
    keys  = pressedKeys g
    space = KeySpace `elem` keys
    up    = KeyUp `elem` keys
    -- down  = KeyDown `elem` keys
    left  = KeyLeft `elem` keys
    right = KeyRight `elem` keys
    shft = KeyShiftL `elem` keys
    none = not (space||up||left||right)
    phys  = physics typ
    grounded = gnd phys == GROUNDED
    (ax,ay) = acc phys
    movl
      | grounded&&left = -300
      | not grounded&&left= -100
      | otherwise = 0
    movr
      | grounded&&right = 300
      | not grounded&&right= 100
      | otherwise = 0
    jump
      | grounded&&jmpt>0 = (-grav)
      | jmpt>0           = 0.5*(-grav)
      | otherwise = 0
    phys' = phys {acc = acc',mxv = mv'}
    mv'  = if shft then (700,800) else (300,800)
    acc'
      | none = (0,0)
      | jmpt < 0 = (ax+movl+movr,0)
      | otherwise = (ax+movl+movr,ay+jump)
    jmpt = pJumpTime pl
    jmpt'
      | up&&grounded      = 0.2
      | space&&grounded   = 0.2
      | not (up||space)   = 0
      | otherwise         = jmpt

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
inHitbox (x1,y1) (x2,y2) (MkHB w h) = x1>lp && y1>bp && x1<rp && y1<tp
  where
    (lp,rp) = (x2-(w/2)+2,x2+(w/2)-2)
    (bp,tp) = (y2-(h/2),y2+(h/2)+1)

intersects :: Point -> Hitbox -> Point -> Hitbox -> Bool
intersects (x1,y1) (MkHB w1 h1) p2 hb2 =
  inHitbox c1 p2 hb2 || inHitbox c2 p2 hb2 || inHitbox c3 p2 hb2 || inHitbox c4 p2 hb2
    where
      c1 = (x1+(w1/2),y1+(h1/2))
      c2 = (x1-(w1/2),y1+(h1/2))
      c3 = (x1+(w1/2),y1-(h1/2))
      c4 = (x1-(w1/2),y1-(h1/2))

blockCheck :: GameState -> Entity -> Entity
blockCheck g e@(MkEntity _ p _) = foldr blockCheck' e {physics = p {gnd=AIRBORNE}} blks
  where
    blks = blocks g
    blockCheck' ::  Block -> Entity -> Entity
    blockCheck' blk e@(MkEntity _ obj _)
      | intersects opos ohb ppos phb = e {physics=obj'}
      | otherwise = e {physics=obj}
      where
        opos@(ox,oy) = pos obj
        ppos@(px,py) = gridPos (bPos blk)
        (vx,vy) = vel obj
        (ax,ay) = acc obj
        ohb@(MkHB ow oh)  = (\(MkHB c d) -> MkHB (c*scaling) (d*scaling)) (htb obj)
        phb@(MkHB pw ph)  = (\(MkHB c d) -> MkHB (c*scaling) (d*scaling)) (bHitbox blk)
        obj'
          | abs (ox-px)>abs (oy-py) = sides
          | oy < py               = obj {pos = ydown, vel = (vx,-vy), acc = (ax,0)}
          | otherwise             = obj {gnd = GROUNDED, pos = yup}
        sides
          | ox < px = obj   {pos = xleft, vel = (0,vy), acc = (0,ay)}
          | otherwise = obj {pos = xright, vel = (0,vy), acc = (0,ay)}
        yup = (ox,oy+((oh/2)+(ph/2)-abs (oy-py)))
        ydown = (ox,oy-((oh/2)+(ph/2)-abs (py-oy)))
        xleft = (ox-((ow/2)+(pw/2)-abs (ox-px))+2,oy)
        xright= (ox+((ow/2)+(pw/2)-abs (ox-px))-2,oy)

platformCheck :: GameState -> Entity -> Entity
platformCheck g e@(MkEntity _ p _) = foldr platformCheck' e plats
  where
    plats = platforms g
    platformCheck' ::  Platform -> Entity -> Entity
    platformCheck' plt e@(MkEntity _ obj _)
      | intersects opos ohb ppos phb = e {physics=obj'}
      | otherwise = e {physics=obj}
      where
        opos@(ox,oy) = pos obj
        ppos@(px,py) = gridPos (pfPos plt)
        (vx,vy) = vel obj
        (ax,ay) = acc obj
        ohb@(MkHB ow oh)  = (\(MkHB c d) -> MkHB (c*scaling) (d*scaling)) (htb obj)
        phb@(MkHB pw ph)  = (\(MkHB c d) -> MkHB (c*scaling) (d*scaling)) (pfHitbox plt)
        obj'
          | abs (ox-px)>abs (oy-py) = sides
          | oy < py               = obj {pos = ydown, vel = (vx,-vy), acc = (ax,0)}
          | otherwise             = obj {gnd = GROUNDED, pos = yup}
        sides
          | ox < px = obj   {pos = xleft, vel = (0,vy), acc = (0,ay)}
          | otherwise = obj {pos = xright, vel = (0,vy), acc = (0,ay)}
        yup = (ox,oy+((oh/2)+(ph/2)-abs (oy-py)))
        ydown = (ox,oy-((oh/2)+(ph/2)-abs (py-oy)))
        xleft = (ox-((ow/2)+(pw/2)-abs (ox-px))+2,oy)
        xright= (ox+((ow/2)+(pw/2)-abs (ox-px))-2,oy)

collisionCheck :: Entity -> Entity --TODO: SUPER BAD FUNCTION GARBAGE PLS FIXXXXXX MEEEEE
collisionCheck e@(MkEntity _ p _) = e {physics = p {pos = (x',y), vel = (vx',vy), acc = (ax',ay)}}
  where
    (x,y)   = pos p
    (vx,vy) = vel p
    (ax,ay) = acc p
    g       = gnd p
    w = (\(MkHB c d) -> c*scaling) (htb p)
    (l,r) = (x-(w/2),x+(w/2))
    (vx',ax') = if r > fst uppbound || l < fst lowbound then (0,0) else (vx,ax)
    x'
      | r > fst uppbound = x-1
      | l < fst lowbound = x+1
      | otherwise = x