module Model.Block where

import Model.Basic
import Model.Item

data Block = MkBlock
    {   bType :: Entity
    ,   bContents :: Item
    ,   bPos :: GridIndex
    } deriving (Show,Eq)