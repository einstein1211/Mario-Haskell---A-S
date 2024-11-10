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

-- Entitiy update function, handles updates to entities such as: 
-- check whether entity lives, 
-- check if an entity should spawn, 
-- check if an entity should move with the window
entityUpdate :: GameState -> GameState
entityUpdate g =  filterAlive $ filterSpawn $ windowShift g
  where
    windowShift gs
    -- Once the sliding window shifts, load new enemies into the enemy list
      | not (windowShifted gs) =
        gs {
          enemies = enemies gs ++ map (scaleTo es) (Map.foldr (\c ac -> getEntries c++ac) [] (slidingWindow gs)),
          slidingWindow = Map.foldrWithKey (\k c ac -> Map.insert k (deleteEntries PlayerEntry c) ac) Map.empty $ Map.foldrWithKey (\k c ac -> Map.insert k (deleteEntries EnemyEntry c) ac) Map.empty (slidingWindow gs),
          windowShifted = True
          }
      | otherwise = gs
    -- Filter dead entities out of their respective lists
    filterAlive gs =
      gs {
        -- If an item of enemy is killed, update the score
        score = score g + (length (filter (not.isAlive) (enemies gs)) * 100) + (length (filter (not.isAlive) (items gs)) * 100),
        players = filter isAlive (map (playerState es) (players gs)),
        enemies  = filter isAlive (enemies gs),
        items    = filter isAlive (items gs),
        slidingWindow = Map.foldrWithKey (\k c ac -> Map.insert k (checkAlive c) ac) Map.empty (slidingWindow gs)
        }
    checkAlive (MkColumn tiles) = MkColumn $ foldr f [] tiles
      where
        f t@(MkTile s (MkBlkChunk b)) ac
          | isAlive b = t : ac
          | otherwise = MkTile s NoChunk : ac
        f t ac = t : ac
    -- If an item is spawned, load it into the items list, erase it from the sliding window afterwards
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

-- deleteEntriesFromWindow :: Level -> Level
-- deleteEntriesFromWindow = Map.foldrWithKey (\k c ac -> Map.insert k (deleteEntries EnemyEntry c) ac) Map.empty

-- Checks what type of movement the player is in and updates the parameters
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
    p' 
      | not grounded                      = scaleTo s p {pMovement = JUMPING, pType= typ {physics = phys {htb = htbJump}}}
      | pMovement p == CROUCHING          = scaleTo s p {pMovement = CROUCHING, pType= typ {physics = phys {htb = htbCrouch}}}
      | vx==0                             = scaleTo s p {pMovement = STANDING, pType= typ {physics = phys {htb = htbGround}}}
      | otherwise                         = scaleTo s p {pMovement = WALKING, pType= typ {physics = phys {htb = htbGround}}}

-- Check per enemy type the interactions that can occur
entityInteractions :: Float -> GameState -> GameState
entityInteractions s g =
  g {
    players = map pve (map pvi (players g)),
    enemies = map evp (map eve (enemies g)),
    items   = map ivp (items g),
    slidingWindow = Map.foldrWithKey (\k c ac -> Map.insert k (bvp c) ac) Map.empty (slidingWindow g)
  }
  where
    pve p = foldr playerVsEnemy p (enemies g)
    pvi p = foldr (playerVsItem (windowScale g)) p (items g)
    evp e = foldr enemyVsPlayer e (players g)
    eve e = foldr enemyVsEnemy e (filter (/=e) (enemies g))
    ivp i = foldr (itemVsPlayer (windowScale g)) i (players g)
    bvp :: Column -> Column --Block vs Player interactions
    bvp (MkColumn tiles) = MkColumn $ foldr f [] tiles
      where
        f (MkTile spawn (MkBlkChunk b)) ac = MkTile spawn (MkBlkChunk (bvp' b)) : ac
        f t ac = t : ac
        bvp' b = foldr (blockVsPlayer g) b (players g)

-- Handles a player when they hit an enemy
playerVsEnemy :: Enemy -> Player -> Player
playerVsEnemy e p = newp
  where
    newp
      | intersects ppos phb epos ehb && pInvTime p <= 0 = p'
      | intersects ppos phb epos ehb && pInvTime p > 0  = p
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
      | abs (px-ex) < abs (py-ey) && (py > ey) = p {pType = ent {physics = pphys {vel = (vx,500), gnd = GROUNDED}}}
      | otherwise = damage

    damage = case pPower p of
      SMALL | pInvTime p <= 0 -> kill p 
      _ -> p {pPower = SMALL, pInvTime = 0.7}  
     
-- Handles an enemy once they are hit by a player
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
      | otherwise = e

-- Handles enemy colissions, so they turn around
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

-- Handles a player when they collect an item
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

-- Handles an item once it is collected by a player
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

-- Handles blocks that are hit by the player
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
      | otherwise = b
    hit =
      case bContents b of
        NOITEM ->
          case bType b of
            BRICK       -> if pPower p == BIG then kill b else b
            QBLOCK      -> b {bType = EMPTYBLOCK}
            EMPTYBLOCK  -> b
            HIDDENBLOCK -> b {bType = EMPTYBLOCK}
        _      ->
          case bType b of
            BRICK       -> if pPower p == BIG then kill b {bSpawn = True} else b
            QBLOCK      -> b {bType = EMPTYBLOCK, bSpawn = True}
            EMPTYBLOCK  -> b
            HIDDENBLOCK -> b {bType = EMPTYBLOCK, bSpawn = True}