module Model.Item where

import Model.Basic

instance IsAlive Item where
    isAlive i = alive (iType i) == ALIVE

-- | Data descriving objects in Game (Coins & Powerups)
data Item = NOITEM | MkItem
    {   iType :: Entity
    ,   iPos :: GridIndex
    } deriving (Show,Eq)

coin :: Item
coin = MkItem 
    { iType = MkEntity {entity = MkItemType COIN, 
                        physics = initPhysicsCoin,
                        alive = ALIVE}
    , iPos = MkGrid 2 7
    }

mushroom :: Item
mushroom = MkItem 
    { iType = MkEntity {entity = MkItemType MUSHROOM, 
                        physics = initPhysicsMushroom,
                        alive = ALIVE}
    , iPos = MkGrid 3 4 -- what do 
    }

initPhysicsCoin :: Physics
initPhysicsCoin = MkPhysics 
    {   pos = (60.0,0.0)
    ,   vel = (0.0,0.0)
    ,   mxv = (3000,3000)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = MkHB (10*scaling) (14*scaling)    
    ,   dir = RIGHT
    }

initPhysicsMushroom :: Physics
initPhysicsMushroom = MkPhysics 
    {   pos = (300.0,0.0)
    ,   vel = (100.0,0.0)
    ,   mxv = (3000,3000)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = MkHB (14*scaling) (16*scaling)    
    ,   dir = RIGHT
    }
