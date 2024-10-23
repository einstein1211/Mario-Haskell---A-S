module Controller.Entity where

import Model.Basic
import Model.Player
import Model.Enemy
import Model.Item
import Model.Block
import Model.Model
import Model.Platform
import Controller.Physics
import View.Scaling

entityUpdate :: GameState -> GameState
entityUpdate g 
  | isScaled g =
    g {players = filter isAlive (map playerState (players g)),
      enemies  = filter isAlive (enemies g),
      items    = filter isAlive (items g),
      blocks   = filter isAlive (blocks g)
      }
  | otherwise =
    g {players  = map scaleTo (players g),
      enemies   = map scaleTo (enemies g),
      items     = map scaleTo (items g),
      blocks    = map scaleTo (blocks g),
      platforms = map scaleTo (platforms g),
      isScaled  = True
      }
  

playerState :: Player -> Player
playerState p = p'
  where
    typ = pType p
    phys = physics typ
    hitbox = htb phys
    (vx,vy) = vel phys
    grounded = gnd phys == GROUNDED

    p'
      | not grounded  = scaleTo p {pMovement = JUMPING, pType= typ {physics = phys {htb = (MkHB 16 16)}}}
      | vx==0         = scaleTo p {pMovement = STANDING, pType= typ {physics = phys {htb = (MkHB 12 16)}}}
      | otherwise     = scaleTo p {pMovement = RUNNING, pType= typ {physics = phys {htb = (MkHB 12 16)}}}

entityInteractions :: Float -> GameState -> GameState
entityInteractions s g =
    g {
      players = map pve (map pvi (players g)),
      enemies = map evp (enemies g),
      items   = map ivp (items g),
      blocks  = map bvp (blocks g)
    }
    where
        pve p = foldr playerVsEnemy p (enemies g)
        pvi p = p
        evp e = foldr enemyVsPlayer e (players g)
        ivp i = i
        bvp b = foldr blockVsPlayer b (players g)

playerVsEnemy :: Enemy -> Player -> Player
playerVsEnemy e p = newp
  where
    newp
      | intersects ppos phb epos ehb = p'
      | otherwise                    = p
    ppos@(px,py)    = pos pphys
    phb@(MkHB _ ph) = htb pphys
    pphys           = physics ent
    (ax,ay)         = acc pphys
    (vx,vy)         = vel pphys
    epos@(ex,ey)    = pos ephys
    ehb@(MkHB _ eh) = htb ephys
    ephys           = physics (eType e)
    ent             = pType p
    p' 
      | abs (px-ex) < abs (py-ey) && (py > ey) = p {pType = ent {physics = pphys {vel = (vx,500),gnd = GROUNDED}}}
      -- | abs (px-ex) < abs (py-ey) && (py > (ey-5)) = p {pType = ent {physics = pphys {pos = yup,gnd = GROUNDED}}}
      | otherwise = damage
    -- yup = (px,py+((ph/2)+(eh/2)-abs (py-ey)))
    damage =
      case pPower p of
        SMALL -> p {pType = ent {alive = DEAD}}
        _     -> p {pPower = SMALL}

enemyVsPlayer :: Player -> Enemy -> Enemy
enemyVsPlayer p e = newe
  where
    newe
      | intersects ppos phb epos ehb = e'
      | otherwise                    = e
    ppos@(px,py)    = pos pphys
    phb@(MkHB _ ph) = htb pphys
    pphys           = physics (pType p)
    (ax,ay)         = acc pphys
    (vx,vy)         = vel pphys
    epos@(ex,ey)    = pos ephys
    ehb@(MkHB _ eh) = htb ephys
    ephys           = physics ent
    ent             = eType e
    e' 
      | abs (px-ex) < abs (py-ey) && (py > ey) = e {eType = ent {alive = DEAD}}
      -- | abs (px-ex) < abs (py-ey) && (py > (ey-5)) = p {pType = ent {physics = pphys {pos = yup,gnd = GROUNDED}}}
      | otherwise = e
-- entityInteract :: Entity -> Entity -> Entity
-- entityInteract

blockVsPlayer :: Player -> Block -> Block
blockVsPlayer p b = newb
  where
    newb
      | intersects ppos phb bpos bhb = b'
      | otherwise                    = b
    ppos@(px,py)    = pos pphys
    phb@(MkHB _ ph) = htb pphys
    pphys           = physics (pType p)
    (ax,ay)         = acc pphys
    (vx,vy)         = vel pphys
    bpos@(bx,by)    = gridPos $ pfPos pf
    bhb@(MkHB _ bh) = pfHitbox pf
    pf              = bPlatform b
    hidden = bType b == HIDDENBLOCK
    b' 
      | abs (px-bx) < abs (py-by) && (py < by) && hidden && vy > 0 = hit
      | abs (px-bx) < abs (py-by) && (py < by) && not hidden= hit
      -- | abs (px-ex) < abs (py-ey) && (py > (ey-5)) = p {pType = ent {physics = pphys {pos = yup,gnd = GROUNDED}}}
      | otherwise = b
    hit =
      case bType b of
        BRICK       -> b {bAlive = DEAD}
        QBLOCK      -> b {bType = EMPTYBLOCK}
        EMPTYBLOCK  -> b
        HIDDENBLOCK -> b {bType = EMPTYBLOCK}