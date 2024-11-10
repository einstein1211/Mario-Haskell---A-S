{-# LANGUAGE InstanceSigs #-}
module Model.Item where

import Model.Basic
    
instance PhysicsFunctions Item where
    getHitbox :: Item -> Hitbox
    getHitbox i = htb $ physics $ iType i
    isAlive :: Item -> Bool
    isAlive i = alive (iType i) == ALIVE
    kill i = i {iType = (iType i) {alive = DEAD}}

-- | Data descriving objects in Game (Coins & Powerups)
data Item = NOITEM | MkItem
    {   iType :: Entity
    } deriving (Show,Eq)