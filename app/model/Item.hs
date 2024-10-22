module Model.Item where

import Model.Basic

instance IsAlive Item where
    isAlive i = alive (iType i) == ALIVE
-- | Data descriving objects in Game (Coins & Powerups)
data Item = NOITEM | MkItem
    {   iType :: Entity
    } deriving (Show,Eq)