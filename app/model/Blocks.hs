module Model.Blocks where

import Model.Basic
import Model.Items
-- import qualified Model.Model (Item)

data BlckType   = BRICK | QBLOCK     | EMPTYBLOCK | INVISBLOCK
    deriving (Show,Eq)

blockhb :: Hitbox
blockhb = HB 16 16

data Block = Block
    {   bType :: BlckType
    ,   bHitbox :: Hitbox
    ,   bPos :: GridIndex
    ,   bAlive :: IsAlive
    ,   bContents :: Item
    } deriving (Show,Eq)

brick :: Block
brick = Block
    {   bType = BRICK
    ,   bHitbox = HB 16 16
    ,   bPos = GRD 2 2
    ,   bAlive = ALIVE
    ,   bContents = NOITEM
    }

qblock :: Block
qblock = Block
    {   bType = QBLOCK
    ,   bHitbox = HB 16 16
    ,   bPos = GRD 3 2
    ,   bAlive = ALIVE
    ,   bContents = NOITEM
    }

empblock :: Block
empblock = Block
    {   bType = EMPTYBLOCK
    ,   bHitbox = HB 16 16
    ,   bPos = GRD 4 2
    ,   bAlive = ALIVE
    ,   bContents = NOITEM
    }