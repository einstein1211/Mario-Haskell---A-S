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
    moveBy :: (Float, Float) -> Item -> Item
    moveBy _ NOITEM = NOITEM
    moveBy (xoff,yoff) i = i {iType = (iType i) {physics = (physics (iType i)) {pos = (xoff+fst(getPos i),yoff+snd(getPos i))}}}
    kill i = i {iType = (iType i) {alive = DEAD}}

-- | Data descriving objects in Game (Coins & Powerups)
data Item = NOITEM | MkItem
    {   iType :: Entity
    -- ,   iPos :: GridIndex
    } deriving (Show,Eq)

-- Make item functions, places item at given location
makeMushroom :: Point -> Item
makeMushroom (x,y) = MkItem
    {   iType = MkEntity
                {entity = MkItemType MUSHROOM,
                physics = mushroomPhys {pos = (x,y)},
                alive = ALIVE}
    }

makeCoin :: Point -> Item
makeCoin (x,y) = MkItem
    {   iType = MkEntity
                {entity = MkItemType COIN,
                physics = coinPhys {pos = (x,y)},
                alive = ALIVE}
    }

-- Initial physics for Items
coinPhys :: Physics
coinPhys = MkPhysics 
    {   pos = (0.0,0.0)
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
