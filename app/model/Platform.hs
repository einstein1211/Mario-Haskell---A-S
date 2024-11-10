module Model.Platform where

import Model.Basic
import Graphics.Gloss
data PlatformType    = DIRT  | STAIR     | PIPEL    | PIPER  | PIPETL  | PIPETR | BLOCK | FLAGPOLE | FLAGTOP | FLAG
    deriving (Show,Eq)

instance PhysicsFunctions Platform where
    getPos = pfPos
    getHitbox = pfHitbox
    moveBy (xoff,yoff) p = p{pfPos = (xoff+fst(getPos p),yoff+snd(getPos p))}

instance Show Platform where
    show (MkPlatform pft _ (x,y)) =
        show pft ++ " (" ++ show x ++ "," ++ show y ++ ")"

-- Platform datatype, holds its hitbox and position
data Platform = MkPlatform
    {   pfType :: PlatformType
    ,   pfHitbox :: Hitbox
    ,   pfPos :: Point
    } deriving (Eq)

-- Standard platform hitbox
platformHB :: Hitbox
platformHB = MkHB 16 16
