module Model.Block where

import Model.Basic
import Model.Item

blockhb :: Hitbox
blockhb = MkHB 16 16

-- data Block = MkBlock
--     {   bType :: Entity
--     ,   bContents :: Item
--     ,   bPos :: GridIndex
--     } deriving (Show,Eq)

data Block = MkBlock
    {   bType :: BlockType
    ,   bHitbox :: Hitbox
    ,   bPos :: GridIndex
    ,   bAlive :: IsAlive
    ,   bContents :: Item
    } deriving (Show,Eq)

brick :: Block
brick = MkBlock
    {   bType = BRICK
    ,   bHitbox = blockhb
    ,   bPos = MkGrid 2 2
    ,   bAlive = ALIVE
    ,   bContents = NOITEM
    }

qblock :: Block
qblock = MkBlock
    {   bType = QBLOCK
    ,   bHitbox = blockhb
    ,   bPos = MkGrid 3 2
    ,   bAlive = ALIVE
    ,   bContents = NOITEM
    }

empblock :: Block
empblock = MkBlock
    {   bType = EMPTYBLOCK
    ,   bHitbox = blockhb
    ,   bPos = MkGrid 4 2
    ,   bAlive = ALIVE
    ,   bContents = NOITEM
    }