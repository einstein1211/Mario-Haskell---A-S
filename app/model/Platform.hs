module Model.Platform where

import Model.Basic

data PlatformType    = DIRT  | STAIR     | PIPEL    | PIPER  | PIPETL  | PIPETR | BLOCK
    deriving (Show,Eq)

instance GetHitbox Platform where
    getHitbox p = pfHitbox p

data Platform = MkPlatform
    {   pfType :: PlatformType
    ,   pfHitbox :: Hitbox
    ,   pfPos :: GridIndex
    } deriving (Show,Eq)

platformHB :: Hitbox
platformHB = MkHB 16 16

makeFloor :: [Platform]
makeFloor = makeFloor' 15
    where
        makeFloor' :: Int -> [Platform]
        makeFloor' (-1) = []
        makeFloor' x    = dirt : makeFloor' (x-1) 
            where
                dirt    = MkPlatform {pfType = DIRT, pfHitbox = platformHB, pfPos = MkGrid (fromIntegral x) 11}

stair :: Platform
stair = MkPlatform
    {   pfType = STAIR
    ,   pfHitbox = platformHB
    ,   pfPos = MkGrid 12 7
    }

stair2 :: Platform
stair2 = MkPlatform
    {   pfType = STAIR
    ,   pfHitbox = platformHB
    ,   pfPos = MkGrid 13 7
    }

pipe1 :: Platform
pipe1 = MkPlatform
    {   pfType = PIPETL
    ,   pfHitbox = platformHB
    ,   pfPos = MkGrid 7 9
    }

pipe2 :: Platform
pipe2 = MkPlatform
    {   pfType = PIPETR
    ,   pfHitbox = platformHB
    ,   pfPos = MkGrid 8 9
    }

pipe3 :: Platform
pipe3 = MkPlatform
    {   pfType = PIPEL
    ,   pfHitbox = platformHB
    ,   pfPos = MkGrid 7 10
    }

pipe4 :: Platform
pipe4 = MkPlatform
    {   pfType = PIPER
    ,   pfHitbox = platformHB
    ,   pfPos = MkGrid 8 10
    }
