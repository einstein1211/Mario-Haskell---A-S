module Model.Platforms where

import Model.Basic
import View.Images (dirt1)

data PltType    = DIRT  | STAIR     | PIPEL    | PIPER  | PIPETL  | PIPETR
    deriving (Show,Eq)

data Platform = Platform
    {   pltType :: PltType
    ,   pltHitbox :: Hitbox
    ,   pltPos :: GridIndex
    } deriving (Show,Eq)

makeFloor :: [Platform]
makeFloor = makeFloor' 15
    where
        makeFloor' :: Int -> [Platform]
        makeFloor' (-1) = []
        makeFloor' x    = dirt : makeFloor' (x-1) 
            where
                dirt    = Platform {pltType = DIRT, pltHitbox = HB 16 16, pltPos = GRD (fromIntegral x) 11}

stair :: Platform
stair = Platform
    {   pltType = STAIR
    ,   pltHitbox = HB 16 16
    ,   pltPos = GRD 12 7
    }

pipe1 :: Platform
pipe1 = Platform
    {   pltType = PIPETL
    ,   pltHitbox = HB 16 16
    ,   pltPos = GRD 4 9
    }

pipe2 :: Platform
pipe2 = Platform
    {   pltType = PIPETR
    ,   pltHitbox = HB 16 16
    ,   pltPos = GRD 5 9
    }

pipe3 :: Platform
pipe3 = Platform
    {   pltType = PIPEL
    ,   pltHitbox = HB 16 16
    ,   pltPos = GRD 4 10
    }

pipe4 :: Platform
pipe4 = Platform
    {   pltType = PIPER
    ,   pltHitbox = HB 16 16
    ,   pltPos = GRD 5 10
    }
