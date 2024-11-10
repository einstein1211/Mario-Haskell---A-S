{-# LANGUAGE InstanceSigs #-}
module Model.Block where

import Model.Basic
import Model.Item
import Model.Platform

blockhb :: Hitbox
blockhb = MkHB 16 16

instance PhysicsFunctions Block where
    getPos b = pfPos $ bPlatform b
    getHitbox b = pfHitbox $ bPlatform b
    isAlive b = bAlive b == ALIVE
    moveBy :: (Float, Float) -> Block -> Block
    moveBy offset b@MkBlock {bPlatform = p} = b{bPlatform = moveBy offset p}
    kill b = b {bAlive = DEAD}

-- instance GridIndexFunctions Block where
--     changeGridIndex grd b@(MkBlock _ pl _ _) = b {bPlatform = pl {pfPos = grd}}
--     getGridIndex b = pfPos $ bPlatform b

data Block = MkBlock
    {   bType :: BlockType
    ,   bPlatform :: Platform
    ,   bAlive :: Alive
    ,   bContents :: Item
    ,   bSpawn :: Bool
    } deriving (Show,Eq)

-- brick :: Block
-- brick = MkBlock
--     {   bType = BRICK
--     ,   bPlatform = MkPlatform {pfType = BLOCK, pfHitbox = blockhb, pfPos = makeGridPos (2,6) 4}
--     ,   bAlive = ALIVE
--     ,   bContents = NOITEM
--     }

-- qblock :: Block
-- qblock = MkBlock
--     {   bType = QBLOCK
--     ,   bPlatform = MkPlatform {pfType = BLOCK, pfHitbox = blockhb, pfPos = makeGridPos (12,6) 4}
--     ,   bAlive = ALIVE
--     ,   bContents = NOITEM
--     }

-- empblock :: Block
-- empblock = MkBlock
--     {   bType = EMPTYBLOCK
--     ,   bPlatform = MkPlatform {pfType = BLOCK, pfHitbox = blockhb, pfPos = makeGridPos (4,6) 4}
--     ,   bAlive = ALIVE
--     ,   bContents = NOITEM
--     }

-- hidblock :: Block
-- hidblock = MkBlock
--     {   bType = HIDDENBLOCK
--     ,   bPlatform = MkPlatform {pfType = BLOCK, pfHitbox = blockhb, pfPos = makeGridPos (5,6) 4}
--     ,   bAlive = ALIVE
--     ,   bContents = NOITEM
--     }