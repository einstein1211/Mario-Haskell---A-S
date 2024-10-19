module Model.Items where

import Model.Basic

data ItmType    = COIN  | HIDDENCOIN| MUSHROOM | FIREFLOWER | STAR 
    deriving (Show,Eq)

-- | Data descriving objects in Game (Coins & Powerups)
data Item = NOITEM | Item
    {   iType :: ItmType
    ,   iPhysics :: Physics
    ,   iAlive :: IsAlive
    } deriving (Show,Eq)