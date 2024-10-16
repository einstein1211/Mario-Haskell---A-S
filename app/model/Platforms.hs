module Model.Platforms where

import Model.Model

data PltType    = DIRT  | STAIR     | PIPEL    | PIPER  | PIPETL  | PIPETR
    deriving (Show,Eq)

data Platform = Platform
    {   pltType :: PltType
    ,   pltHitbox :: Hitbox
    ,   pltPosition :: GridIndex 
    } deriving (Show,Eq)