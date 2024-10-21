module Model.Blocks where

import Model.Basic
import Model.Items
-- import qualified Model.Model (Item)

data BlckType   = BRICK | QBLOCK     | EMPTYBLOCK | INVISBLOCK
    deriving (Show,Eq)



data Block = Block
    {   bType :: BlckType
    ,   bHitbox :: Hitbox
    ,   bPos :: GridIndex
    ,   bAlive :: IsAlive
    ,   bContents :: Item
    } deriving (Show,Eq)

