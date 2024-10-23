module Model.Item where

import Model.Basic

instance IsAlive Item where
    isAlive i = alive (iType i) == ALIVE

instance GetHitbox Item where
    getHitbox i = htb $ physics $ iType i

-- | Data descriving objects in Game (Coins & Powerups)
data Item = NOITEM | MkItem
    {   iType :: Entity
    } deriving (Show,Eq)