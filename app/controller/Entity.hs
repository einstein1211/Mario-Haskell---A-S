module Controller.Entity where

import Model.Basic
import Model.Player
import Model.Enemy
import Model.Item
import Model.Block
import Model.Model
import Model.Platform
import Model.Level
import Controller.Physics
import View.Scaling

import qualified Data.Map as Map

entityUpdate :: GameState -> GameState
entityUpdate g =  scale $ windowShift $ noReScale g
    where
      noReScale gs
        | not (reScaled gs) =
          gs {players  = map (scaleTo ws) (players g),
            enemies   = map (scaleTo ws) (enemies g),
            items     = map (scaleTo ws) (items g),
            blocks    = map (scaleTo ws) (blocks g),
            platforms = map (scaleTo ws) (platforms g),
            isScaled  = True
            }
        | otherwise =
          gs {players  = map (scaleTo es) (players g),
            enemies   = map (scaleTo es) (enemies g),
            items     = map (scaleTo es) (items g),
            -- blocks    = map (scaleTo es) (blocks g),
            -- platforms = map (scaleTo es) (platforms g),
            isScaled  = True
            }
      windowShift gs
        | not (windowShifted gs) =
          gs {
            -- blocks = Map.foldr (\c ac -> getEntries c++ac) [] (level g), --level for now, must be sliding window
            blocks = map (scaleTo es)$ Map.foldl (\ac c -> getEntries c++ac) [] (slidingWindow g),
            -- platforms = Map.foldr (\c ac -> getEntries c++ac) [] (level g),
            platforms = map (scaleTo es)$ Map.foldl (\ac c -> getEntries c++ac) [] (slidingWindow g),
            windowShifted = True
            -- ,isScaled = False
            }
        | otherwise = gs
      scale gs
        | isScaled gs =
          gs {players = filter isAlive (map (playerState es) (players g)),
            enemies  = filter isAlive (enemies g),
            items    = filter isAlive (items g),
            blocks   = filter isAlive (blocks g)
            }
        | otherwise = gs
      es = entityScale g
      ws = windowScale g
      f t = changeGridIndex (MkGrid (x-1) y) t
          where
            (MkGrid x y) = getGridIndex t


playerState :: Scaling -> Player -> Player
playerState s p = p'
  where
    typ = pType p
    phys = physics typ
    hitbox = htb phys
    (vx,vy) = vel phys
    grounded = gnd phys == GROUNDED

    p'
      | not grounded  = scaleTo s p {pMovement = JUMPING, pType= typ {physics = phys {htb = (MkHB 14 16)}}}
      | vx==0         = scaleTo s p {pMovement = STANDING, pType= typ {physics = phys {htb = (MkHB 12 16)}}}
      | otherwise     = scaleTo s p {pMovement = RUNNING, pType= typ {physics = phys {htb = (MkHB 12 16)}}}

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
-- entityInteract :: Entity -> Entity -> Entity
-- entityInteract

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