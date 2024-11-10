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
    g {players = filter isAlive (map (playerState es) (players g)),
      enemies  = filter isAlive (enemies g),
      items    = filter isAlive (items g),
      blocks   = filter isAlive (blocks g)
      }
  | not (reScaled g) =
    g {players  = map (scaleTo ws) (players g),
      enemies   = map (scaleTo ws) (enemies g),
      items     = map (scaleTo ws) (items g),
      blocks    = map (scaleTo ws) (blocks g),
      platforms = map (scaleTo ws) (platforms g),
      isScaled  = True
      }
  | otherwise =
    g {players  = map (scaleTo 4) (players g),
      enemies   = map (scaleTo 4) (enemies g),
      items     = map (scaleTo 4) (items g),
      blocks    = map (scaleTo 4) (blocks g),
      platforms = map (scaleTo 4) (platforms g),
      isScaled  = True
      }
    where
      es = entityScale g
      ws = windowScale g


playerState :: Scaling -> Player -> Player
playerState s p = p'
  where
    typ = pType p
    phys = physics typ
    hitbox = htb phys
    (vx,vy) = vel phys
    grounded = gnd phys == GROUNDED

    htbGround =
      case pPower p of 
        SMALL -> MkHB 12 16
        BIG   -> MkHB 16 32
    htbJump =
      case pPower p of
        SMALL -> MkHB 14 16
        BIG   -> MkHB 16 32
    htbCrouch = 
      case pPower p of 
        SMALL -> MkHB 12 16
        BIG   -> MkHB 16 22

    p' -- FIX ME
      | not grounded                      = scaleTo s p {pMovement = JUMPING, pType= typ {physics = phys {htb = htbJump}}}
      | pMovement p == CROUCHING          = scaleTo s p {pMovement = CROUCHING, pType= typ {physics = phys {htb = htbCrouch}}}
      | vx==0                             = scaleTo s p {pMovement = STANDING, pType= typ {physics = phys {htb = htbGround}}}
      | otherwise                         = scaleTo s p {pMovement = WALKING, pType= typ {physics = phys {htb = htbGround}}}

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
        evp e = foldr enemyVsPlayer e (players g)
        pvi p = foldr (playervsItem (windowScale g)) p (items g)
        ivp i = foldr (itemVsPlayer (windowScale g)) i (players g)
        bvp b = foldr (blockVsPlayer (windowScale g)) b (players g)

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

playervsItem :: Scaling -> Item -> Player -> Player
playervsItem s i p = newp 
  where
    newp
      | intersects ipos ihb ppos phb = p'
      | otherwise                    = p
    ppos@(px,py)    = pos pphys
    phb@(MkHB _ ph) = htb pphys
    pphys           = physics ent
    ipos@(ix,iy)    = 
      case entity (iType i) of
        MkItemType COIN -> gridPos (iPos i) s
        _               -> pos iphys
    ihb@(MkHB _ ih) = htb iphys
    iphys           = physics (iType i)
    ent             = pType p
    p' = 
      case entity (iType i) of
        MkItemType MUSHROOM -> if pPower p == SMALL then p {pPower = BIG} else p
        _                   -> p

itemVsPlayer :: Scaling -> Player -> Item -> Item
itemVsPlayer s p i = newi
  where
    newi
      | intersects ipos ihb ppos phb = i {iType = ent {alive = DEAD}}
      | otherwise                    = i
    ppos@(px,py)    = pos pphys
    phb@(MkHB _ ph) = htb pphys
    pphys           = physics (pType p)
    ipos@(ix,iy)    = 
      case entity (iType i) of
        MkItemType COIN -> gridPos (iPos i) s
        _               -> pos iphys
    ihb@(MkHB _ ih) = htb iphys
    iphys           = physics ent
    ent             = iType i

blockVsPlayer :: Scaling -> Player -> Block -> Block
blockVsPlayer scale p b = newb
  where
    newb
      | intersects ppos phb bpos bhb = b'
      | otherwise                    = b
    ppos@(px,py)    = pos pphys
    phb@(MkHB _ ph) = htb pphys
    pphys           = physics (pType p)
    (ax,ay)         = acc pphys
    (vx,vy)         = vel pphys
    bpos@(bx,by)    = gridPos (pfPos pf) scale
    bhb@(MkHB _ bh) = pfHitbox pf
    pf              = bPlatform b
    hidden = bType b == HIDDENBLOCK
    b'
      | abs (px-bx) < abs (py-by) && (py < by) && hidden && vy > 0 = hit
      | abs (px-bx) < abs (py-by) && (py < by) && not hidden = hit
      -- | abs (px-ex) < abs (py-ey) && (py > (ey-5)) = p {pType = ent {physics = pphys {pos = yup,gnd = GROUNDED}}}
      | otherwise = b
    hit =
      case bType b of
        BRICK       -> b {bAlive = DEAD}
        QBLOCK      -> b {bType = EMPTYBLOCK}
        EMPTYBLOCK  -> b
        HIDDENBLOCK -> b {bType = EMPTYBLOCK}