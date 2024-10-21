module Model.Item where

import Model.Basic

-- | Data descriving objects in Game (Coins & Powerups)
data Item = NOITEM | MkItem
    {   iType :: Entity
    } deriving (Show,Eq)