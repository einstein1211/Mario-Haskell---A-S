module Model.Item where

import Model.Basic

instance IsAlive Item where
    isAlive i = alive (iType i) == ALIVE

-- | Data descriving objects in Game (Coins & Powerups)
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
    {   pos = (60.0,0.0)
    ,   vel = (0.0,0.0)
    ,   mxv = (3000,3000)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = MkHB (10*scaling) (14*scaling)    
    ,   dir = RIGHT
    }