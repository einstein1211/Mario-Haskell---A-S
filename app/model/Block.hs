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

-- Block datatype, holds the item it can contain as contents
data Block = MkBlock
    {   bType :: BlockType
    ,   bPlatform :: Platform
    ,   bAlive :: Alive
    ,   bContents :: Item
    ,   bSpawn :: Bool
    } deriving (Show,Eq)
