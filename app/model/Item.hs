module Model.Item where

import Model.Basic

-- | Data describing objects in Game (Coins & Powerups)
data Item = NOITEM | MkItem
    {   iType :: Entity
    } deriving (Show,Eq)

coin :: Item
coin = MkItem 
    { iType = MkEntity {entity = MkItemType COIN, 
                        physics = initPhysics,
                        alive = ALIVE}
    }

-- mushroom

-- using enemy physics to test
initPhysics :: Physics
initPhysics = MkPhysics 
    {   pos = (0.0,0.0)
    ,   vel = (200.0,300.0)
    ,   mxv = (3000,3000)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = MkHB 14 16    
    ,   dir = RIGHT
    }