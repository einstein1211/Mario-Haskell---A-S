{-# LANGUAGE InstanceSigs #-}
module Model.Item where

import Model.Basic
import Graphics.Gloss
    
instance PhysicsFunctions Item where
    getPos i = pos $ physics $ iType i
    getHitbox :: Item -> Hitbox
    getHitbox i = htb $ physics $ iType i
    isAlive :: Item -> Bool
    isAlive i = alive (iType i) == ALIVE
    kill i = i {iType = (iType i) {alive = DEAD}}

-- | Data descriving objects in Game (Coins & Powerups)
data Item = NOITEM | MkItem
    {   iType :: Entity
    -- ,   iPos :: GridIndex
    } deriving (Show,Eq)

makeMushroom :: Point -> Item
makeMushroom (x,y) = MkItem
    {   iType = MkEntity
                {entity = MkItemType MUSHROOM,
                physics = mushroomPhys {pos = (x,y)},
                alive = ALIVE}
    }

-- coin :: Item
-- coin = MkItem 
--     { iType = MkEntity {entity = MkItemType COIN, 
--                         physics = initPhysicsCoin,
--                         alive = ALIVE}
--     , iPos = MkGrid 2 7
--     }

-- mushroom :: Item
-- mushroom = MkItem 
--     { iType = MkEntity {entity = MkItemType MUSHROOM, 
--                         physics = initPhysicsMushroom,
--                         alive = ALIVE}
--     , iPos = MkGrid 3 4 -- what do 
--     }

initPhysicsCoin :: Physics
initPhysicsCoin = MkPhysics 
    {   pos = (60.0,0.0)
    ,   vel = (0.0,0.0)
    ,   mxv = (3000,3000)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = MkHB 10 14   
    ,   dir = RIGHT
    }

mushroomPhys :: Physics
mushroomPhys = MkPhysics 
    {   pos = (0.0,0.0)
    ,   vel = (100.0,0.0)
    ,   mxv = (3000,3000)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = MkHB 14 16   
    ,   dir = RIGHT
    }
