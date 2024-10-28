{-# LANGUAGE InstanceSigs #-}
module Model.Level where

import Model.Basic
import Model.Player
import Model.Enemy
import Model.Item
import Model.Block
import Model.Platform
import qualified Data.Map as Map

data Spawn = MkPlayer Player | MkEnemy Enemy | MkItem Item | NoSpawn
    deriving (Show,Eq)
data Chunk = MkBlock Block | MkPlatform Platform | NoChunk
    deriving (Show,Eq)
data Tile = MkTile Spawn Chunk GridIndex
    deriving (Show,Eq)
newtype Column = MkColumn [Tile]
    deriving (Show,Eq)
type ColumnNumber = Int
type Level = Map.Map ColumnNumber Column

class ColumnFunctions a where
    addToColumn :: a -> Column -> Column

instance ColumnFunctions Tile where
    addToColumn :: Tile -> Column -> Column
    addToColumn t@(MkTile _ _ (MkGrid _ y)) (MkColumn tiles)  = MkColumn (take (round(y-1)) tiles ++ t : drop (round y) tiles)

instance ColumnFunctions Int where
    addToColumn :: Int -> Column -> Column
    addToColumn i (MkColumn tiles) = MkColumn (take (i-1) tiles ++ emptyTile (MkGrid 1.0 (fromIntegral i)) : drop i tiles)


emptyTile :: GridIndex -> Tile
emptyTile (MkGrid x y) = MkTile NoSpawn NoChunk (MkGrid x y)

testColumn :: Column
testColumn = emptyColumn

list :: [Int]
list = [1..16]

emptyColumn :: Column
emptyColumn = foldr addToColumn (MkColumn []) list

testLevel :: Level
testLevel = Map.insert 1 testColumn Map.empty