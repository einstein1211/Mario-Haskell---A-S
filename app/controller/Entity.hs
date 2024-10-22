module Controller.Entity where

import Model.Basic
import Model.Player
import Model.Enemy
import Model.Item
import Model.Block
import Model.Model
import Controller.Physics

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
        evp e = e
        ivp i = i
        bvp b = b

playerVsEnemy :: Enemy -> Player -> Player
playerVsEnemy e p = newp
    where
        newp
          | intersects ppos phb epos ehb = p'
          | otherwise                    = p
        ppos@(px,py)  = pos pphys
        phb           = htb pphys
        pphys         = physics ent
        (ax,ay)       = acc pphys
        epos@(ex,ey)  = pos ephys
        ehb           = htb ephys
        ephys         = physics (eType e)
        ent           = pType p
        p'
          | abs (px-ex) > abs (py-ey) || (py < ey) = damage
          | otherwise = p {pType = ent {physics = pphys {acc = (20,ay)}}}
        damage =
          case pPower p of
            SMALL -> p {pType = ent {alive = DEAD}}
            _     -> p {pPower = SMALL}
-- entityInteract :: Entity -> Entity -> Entity
-- entityInteract