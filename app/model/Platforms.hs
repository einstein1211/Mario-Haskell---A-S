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

platformHB :: Hitbox
platformHB = HB 17 16

makeFloor :: [Platform]
makeFloor = makeFloor' 15
    where
        makeFloor' :: Int -> [Platform]
        makeFloor' (-1) = []
        makeFloor' x    = dirt : makeFloor' (x-1) 
            where
                dirt    = Platform {pltType = DIRT, pltHitbox = platformHB, pltPos = GRD (fromIntegral x) 11}

stair :: Platform
stair = Platform
    {   pltType = STAIR
    ,   pltHitbox = platformHB
    ,   pltPos = GRD 12 7
    }

stair2 :: Platform
stair2 = Platform
    {   pltType = STAIR
    ,   pltHitbox = platformHB
    ,   pltPos = GRD 13 7
    }

pipe1 :: Platform
pipe1 = Platform
    {   pltType = PIPETL
    ,   pltHitbox = platformHB
    ,   pltPos = GRD 7 9
    }

pipe2 :: Platform
pipe2 = Platform
    {   pltType = PIPETR
    ,   pltHitbox = platformHB
    ,   pltPos = GRD 8 9
    }

pipe3 :: Platform
pipe3 = Platform
    {   pltType = PIPEL
    ,   pltHitbox = platformHB
    ,   pltPos = GRD 7 10
    }

pipe4 :: Platform
pipe4 = Platform
    {   pltType = PIPER
    ,   pltHitbox = platformHB
    ,   pltPos = GRD 8 10
    }
