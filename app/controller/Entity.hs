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
entityUpdate g =  filterAlive $ filterSpawn $ windowShift g
  where
    windowShift gs
      | not (windowShifted gs) = -- trace "window shifted"
        gs {
          enemies = enemies gs ++ map (scaleTo es) (Map.foldr (\c ac -> getEntries c++ac) [] (slidingWindow gs)),
          slidingWindow = Map.foldrWithKey (\k c ac -> Map.insert k (deleteEntries EnemyEntry c) ac) Map.empty (slidingWindow gs),
          windowShifted = True
          }
      | otherwise = gs
    filterAlive gs =
      gs {
        players = filter isAlive (map (playerState es) (players gs)),
        enemies  = filter isAlive (enemies gs),
        items    = filter isAlive (items gs),
        slidingWindow = Map.foldrWithKey (\k c ac -> Map.insert k (checkAlive c) ac) Map.empty (slidingWindow gs)
        -- blocks   = filter isAlive (blocks gs)
        }
    checkAlive (MkColumn tiles) = MkColumn $ foldr f [] tiles
      where
        f t@(MkTile s (MkBlkChunk b)) ac
          | isAlive b = t : ac
          | otherwise = MkTile s NoChunk : ac
        f t ac = t : ac
    filterSpawn gs =
      gs {
        items = items gs ++ map (scaleTo es) (Map.foldr (\c ac -> getEntries c++ac) [] (slidingWindow gs)),
        slidingWindow = Map.foldrWithKey (\k c ac -> Map.insert k (checkSpawn c) ac) Map.empty $ Map.foldrWithKey (\k c ac -> Map.insert k (deleteEntries ItemEntry c) ac) Map.empty (slidingWindow gs)
        }
    checkSpawn (MkColumn tiles) = MkColumn $ foldr f [] tiles
      where
        f t@(MkTile s c@(MkBlkChunk b@(MkBlock _ _ _ _ spawn))) ac
          | spawn = MkTile (newspawn b) (MkBlkChunk b{bSpawn = False}) : ac
          | otherwise = t : ac
        f t ac = t : ac
        newspawn b = MkItSpawn $ i {iType = (iType i) {physics = (physics (iType i)) {pos = (bx,by+64)}}}
          where
            (bx,by) = getPos b
            i = bContents b
    es = entityScale g
    ws = windowScale g

deleteEntriesFromWindow :: Level -> Level
deleteEntriesFromWindow = Map.foldrWithKey (\k c ac -> Map.insert k (deleteEntries EnemyEntry c) ac) Map.empty

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
    enemies = map evp (map eve (enemies g)),
    items   = map ivp (items g),
    slidingWindow = Map.foldrWithKey (\k c ac -> Map.insert k (bvp c) ac) Map.empty (slidingWindow g)
    -- blocks  = map bvp (blocks g)
  }
  where
    pve p = foldr playerVsEnemy p (enemies g)
    pvi p = foldr (playerVsItem (windowScale g)) p (items g)
    evp e = foldr enemyVsPlayer e (players g)
    eve e = foldr enemyVsEnemy e (filter (/=e) (enemies g))
    ivp i = foldr (itemVsPlayer (windowScale g)) i (players g)
    -- bvp b = foldr (blockVsPlayer (windowScale g)) b (players g) 
    bvp :: Column -> Column --Block vs Player interactions
    bvp (MkColumn tiles) = MkColumn $ foldr f [] tiles
      where
        f (MkTile spawn (MkBlkChunk b)) ac = MkTile spawn (MkBlkChunk (bvp' b)) : ac
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
        SMALL -> kill p
        _     -> p {pPower = SMALL}

enemyVsPlayer :: Player -> Enemy -> Enemy
enemyVsPlayer p e = newe
  where
    newe
      | intersects ppos phb epos ehb = e'
      | otherwise                    = e
    ppos@(px,py)    = getPos p
    phb@(MkHB _ ph) = htb pphys
    pphys           = physics (pType p)
    (ax,ay)         = acc pphys
    (vx,vy)         = vel pphys
    epos@(ex,ey)    = pos ephys
    ehb@(MkHB _ eh) = htb ephys
    ephys           = physics ent
    ent             = eType e
    e'
      | abs (px-ex) < abs (py-ey) && (py > ey) = kill e
      -- | abs (px-ex) < abs (py-ey) && (py > (ey-5)) = p {pType = ent {physics = pphys {pos = yup,gnd = GROUNDED}}}
      | otherwise = e

enemyVsEnemy :: Enemy -> Enemy -> Enemy
enemyVsEnemy e2 e1 = newe
  where
    newe
      | intersects e1pos e1hb e2pos e2hb = e1'
      | otherwise                    = e1
    e1pos@(px,py)    = getPos e1
    e1hb@(MkHB _ ph) = htb e1phys
    e1phys           = physics (eType e1)
    e2pos@(ex,ey)    = pos e2phys
    e2hb@(MkHB _ eh) = htb e2phys
    e2phys           = physics (eType e2)
    (vx,vy)          = getVel e1
    e1'
      | getDir e1 == RIGHT = e1 {eType = (eType e1) {physics = e1phys {vel = (-speed,vy), dir = LEFT}}}
      | getDir e1 == LEFT = e1 {eType = (eType e1) {physics = e1phys {vel = (speed,vy), dir = RIGHT}}}
      | otherwise = e1
    speed =
      case eAI e1 of
        EASY    -> 150
        MEDIUM  -> 175
        HARD    -> 200

playerVsItem :: Scaling -> Item -> Player -> Player
playerVsItem s i p = newp
  where
    newp
      | intersects ipos ihb ppos phb = p'
      | otherwise                    = p
    ppos@(px,py)    = pos pphys
    phb@(MkHB _ ph) = htb pphys
    pphys           = physics ent
    ipos@(ix,iy)    =
      case entity (iType i) of
        MkItemType COIN -> getPos i
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
        MkItemType COIN -> getPos i
        _               -> pos iphys
    ihb@(MkHB _ ih) = htb iphys
    iphys           = physics ent
    ent             = iType i

blockVsPlayer :: GameState -> Player -> Block -> Block
blockVsPlayer g p b =
  newb
  where
    newb
      | intersects ppos phb bpos bhb = b'
      | otherwise                    = b
    ppos@(px,py)    = pos pphys
    phb@(MkHB _ ph) = htb pphys
    pphys           = physics (pType p)
    (ax,ay)         = acc pphys
    (vx,vy)         = vel pphys
    bpos@(bx,by)    = pfPos pf
    bhb@(MkHB _ bh) = hitboxScale (pfHitbox pf) (entityScale g)
    pf              = bPlatform b
    hidden = bType b == HIDDENBLOCK
    b'
      | abs (px-bx) < abs (py-by) && (py < by) && hidden && vy > 0 = hit
      | abs (px-bx) < abs (py-by) && (py < by) && not hidden = hit
      -- | abs (px-ex) < abs (py-ey) && (py > (ey-5)) = p {pType = ent {physics = pphys {pos = yup,gnd = GROUNDED}}}
      | otherwise = b
    hit =
      case bContents b of
        NOITEM ->
          case bType b of
            BRICK       -> kill b
            QBLOCK      -> b {bType = EMPTYBLOCK}
            EMPTYBLOCK  -> b
            HIDDENBLOCK -> b {bType = EMPTYBLOCK}
        _      ->
          case bType b of
            BRICK       -> kill b {bSpawn = True}
            QBLOCK      -> b {bType = EMPTYBLOCK, bSpawn = True}
            EMPTYBLOCK  -> b
            HIDDENBLOCK -> b {bType = EMPTYBLOCK, bSpawn = True}