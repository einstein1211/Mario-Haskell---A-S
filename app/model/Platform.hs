module Model.Platform where

import Model.Basic
import Graphics.Gloss
data PlatformType    = DIRT  | STAIR     | PIPEL    | PIPER  | PIPETL  | PIPETR | BLOCK
    deriving (Show,Eq)

instance PhysicsFunctions Platform where
    getPos = pfPos
    getHitbox = pfHitbox
    moveBy (xoff,yoff) p = p{pfPos = (xoff+fst(getPos p),yoff+snd(getPos p))}

-- instance GridIndexFunctions Platform where
--     changeGridIndex grd pl = pl {pfPos = grd}
--     getGridIndex = pfPos

instance Show Platform where
    show (MkPlatform pft _ (x,y)) =
        show pft ++ " (" ++ show x ++ "," ++ show y ++ ")"

data Platform = MkPlatform
    {   pfType :: PlatformType
    ,   pfHitbox :: Hitbox
    ,   pfPos :: Point
    } deriving (Eq)

platformHB :: Hitbox
platformHB = MkHB 16 16

makeFloor :: [Platform]
makeFloor = makeFloor' 15
    where
        makeFloor' :: Int -> [Platform]
        makeFloor' (-1) = []
        makeFloor' x    = dirt : makeFloor' (x-1)
            where
                dirt    = MkPlatform {pfType = DIRT, pfHitbox = platformHB, pfPos = makeGridPos (fromIntegral x,11) 4}

stair :: Platform
stair = MkPlatform
    {   pfType = STAIR
    ,   pfHitbox = platformHB
    ,   pfPos = makeGridPos (12,7) 4
    }

stair2 :: Platform
stair2 = MkPlatform
    {   pfType = STAIR
    ,   pfHitbox = platformHB
    ,   pfPos = makeGridPos (13,7) 4
    }

pipe1 :: Platform
pipe1 = MkPlatform
    {   pfType = PIPETL
    ,   pfHitbox = platformHB
    ,   pfPos = makeGridPos (7,9) 4
    }

pipe2 :: Platform
pipe2 = MkPlatform
    {   pfType = PIPETR
    ,   pfHitbox = platformHB
    ,   pfPos = makeGridPos (8,9) 4
    }

pipe3 :: Platform
pipe3 = MkPlatform
    {   pfType = PIPEL
    ,   pfHitbox = platformHB
    ,   pfPos = makeGridPos (7,10) 4
    }

pipe4 :: Platform
pipe4 = MkPlatform
    {   pfType = PIPER
    ,   pfHitbox = platformHB
    ,   pfPos = makeGridPos (8,10) 4
    }
