module Model.Block where

import Model.Basic
import Model.Item
import Model.Platform

blockhb :: Hitbox
blockhb = MkHB 16 16

instance PhysicsFunctions Block where
    getHitbox b = pfHitbox $ bPlatform b
    isAlive b = bAlive b == ALIVE
    
instance GridIndexFunctions Block where
    changeGridIndex grd b@(MkBlock _ pl _ _) = b {bPlatform = pl {pfPos = grd}}
    getGridIndex b = pfPos $ bPlatform b

data Block = MkBlock
    {   bType :: BlockType
    ,   bPlatform :: Platform
    ,   bAlive :: Alive
    ,   bContents :: Item
    } deriving (Show,Eq)

brick :: Block
brick = MkBlock
    {   bType = BRICK
    ,   bPlatform = MkPlatform {pfType = BLOCK, pfHitbox = blockhb, pfPos = MkGrid 2 6}
    ,   bAlive = ALIVE
    ,   bContents = NOITEM
    }

qblock :: Block
qblock = MkBlock
    {   bType = QBLOCK
    ,   bPlatform = MkPlatform {pfType = BLOCK, pfHitbox = blockhb, pfPos = MkGrid 12 6}
    ,   bAlive = ALIVE
    ,   bContents = NOITEM
    }

empblock :: Block
empblock = MkBlock
    {   bType = EMPTYBLOCK
    ,   bPlatform = MkPlatform {pfType = BLOCK, pfHitbox = blockhb, pfPos = MkGrid 4 6}
    ,   bAlive = ALIVE
    ,   bContents = NOITEM
    }

hidblock :: Block
hidblock = MkBlock
    {   bType = HIDDENBLOCK
    ,   bPlatform = MkPlatform {pfType = BLOCK, pfHitbox = blockhb, pfPos = MkGrid 5 6}
    ,   bAlive = ALIVE
    ,   bContents = NOITEM
    }