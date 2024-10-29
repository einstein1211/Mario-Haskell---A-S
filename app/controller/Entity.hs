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

import Debug.Trace
import qualified Data.Map as Map

entityUpdate :: GameState -> GameState
entityUpdate g =  filterAlive $ windowShift g-- $ noReScale g
    where
      noReScale gs
        | not (reScaled gs) =
          gs {
            players   = map (scaleTo es) (players gs),
            enemies   = map (scaleTo es) (enemies gs),
            items     = map (scaleTo es) (items gs),
            -- blocks    = map (scaleTo es) (blocks gs),
            -- platforms = map (scaleTo es) (platforms gs),
            isScaled  = True
            }
        | otherwise =
          gs {
            players   = map (scaleTo ws) (players gs),
            enemies   = map (scaleTo ws) (enemies gs),
            items     = map (scaleTo ws) (items gs),
            -- blocks    = map (scaleTo es) (blocks gs),
            -- platforms = map (scaleTo es) (platforms g),
            isScaled  = True
            }
      windowShift gs
        | not (windowShifted gs) = 
          gs {
            -- blocks = Map.foldr (\c ac -> getEntries c++ac) [] (level g), --level for now, must be sliding window
            -- blocks = map (scaleTo es) $ Map.foldr (\c ac -> getEntries c++ac) [] (slidingWindow gs),
            -- platforms = Map.foldr (\c ac -> getEntries c++ac) [] (level g),
            platforms = map (scaleTo es) $ Map.foldr (\c ac -> getEntries c++ac) [] (slidingWindow gs),
            windowShifted = True

            -- ,isScaled = False
            }
        | otherwise = gs
      filterAlive gs =
          gs {
            players = filter isAlive (map (playerState es) (players gs)),
            enemies  = filter isAlive (enemies gs),
            items    = filter isAlive (items gs)
            -- blocks   = filter isAlive (blocks gs)
            }
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
    slidingWindow = Map.foldrWithKey (\k c ac -> Map.insert k (bvp c) ac) Map.empty (slidingWindow g)
    -- blocks  = map bvp (blocks g)
  }
  where
    pve p = foldr playerVsEnemy p (enemies g)
    pvi p = p
    evp e = foldr enemyVsPlayer e (players g)
    ivp i = i
    -- bvp b = foldr (blockVsPlayer (windowScale g)) b (players g) 
    bvp :: Column -> Column --Block vs Player interactions
    bvp (MkColumn tiles) = MkColumn $ foldr f [] tiles
      where
        f (MkTile s (MkBlkChunk b) i) ac = (MkTile s (MkBlkChunk (bvp' b)) i) : ac
        f t ac = t : ac 
        bvp' b = foldr (blockVsPlayer g) b (players g)
    -- bvp (MkColumn []) = MkColumn []
    -- bvp (MkColumn (t@(MkTile s (MkBlkChunk b) i):ts)) = 
    --   MkColumn $ (MkTile s (MkBlkChunk (foldr (blockVsPlayer (windowScale g)) b (players g))) i) : bvp (MkColumn ts)
    -- bvp (MkColumn (t:ts)) = MkColumn $ t : bvp (MkColumn ts)
      -- where
      --   f c ac = (blockVsPlayer (windowScale g))

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

blockVsPlayer :: GameState -> Player -> Block -> Block
blockVsPlayer g p b = 
  -- (trace (show b)) newb
  newb
  where
    newb
      | intersects ppos phb bpos bhb = (trace "block hit") b'
      | otherwise                    =  b
    ppos@(px,py)    = pos pphys
    phb@(MkHB _ ph) = htb pphys
    pphys           = physics (pType p)
    (ax,ay)         = acc pphys
    (vx,vy)         = vel pphys
    bpos@(bx,by)    = gridPos (pfPos pf) (windowScale g)
    bhb@(MkHB _ bh) = hitboxScale (pfHitbox pf) (entityScale g)
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