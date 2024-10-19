module Model.Blocks where

import Model.Basic
import Model.Items
-- import qualified Model.Model (Item)

data BlckType   = BRICK | BLOCK     | EMPTYBLOCK | INVISBLOCK
    deriving (Show,Eq)

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