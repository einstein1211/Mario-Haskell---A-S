{-# LANGUAGE InstanceSigs #-}
module Model.Level where

import Model.Basic
import Model.Player
import Model.Enemy
import Model.Item
import Model.Block
import Model.Platform
import qualified Data.Map as Map

data Spawn = MkPlSpawn Player | MkEnSpawn Enemy | MkItSpawn Item | NoSpawn
    deriving (Show,Eq)
data Chunk = MkBlkChunk Block | MkPltChunk Platform | NoChunk
    deriving (Show,Eq)
data Tile = MkTile Spawn Chunk Int
    deriving (Show,Eq)
newtype Column = MkColumn [Tile]
    deriving (Show,Eq)
type ColumnNumber = Int
type Level = Map.Map ColumnNumber Column

class ColumnFunctions a where
    addToColumn :: a -> Column -> Column

instance ColumnFunctions Tile where
    addToColumn :: Tile -> Column -> Column
    addToColumn t@(MkTile _ _ i) (MkColumn tiles)  = MkColumn (take i tiles ++ t : drop (i+1) tiles)

instance ColumnFunctions Int where
    addToColumn :: Int -> Column -> Column
    addToColumn i (MkColumn tiles) = MkColumn (take (i) tiles ++ (emptyTile i) : drop (i+1) tiles)


emptyTile :: Int -> Tile
emptyTile i = MkTile NoSpawn NoChunk i

testColumn :: Column
testColumn = standardColumn 0

list :: [Int]
list = [0..11]

emptyColumn :: Column
emptyColumn = foldl (flip addToColumn) (MkColumn []) list

dirtColumn :: Column
dirtColumn = foldl addDirt emptyColumn list
    where
        addDirt ac c = addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform DIRT platformHB (grid c))) c) ac
        grid x = (MkGrid 0 (fromIntegral x))

standardColumn :: ColumnNumber -> Column
standardColumn cn = addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform DIRT platformHB (MkGrid cn 11))) 11) emptyColumn
-- makePlatform :: GridIndex -> PlatformType -> Platform
-- makePlatform grd plt = MkPlatform
--     {   pfType = plt
--     ,   pfHitbox = platformHB
--     ,   pfPos = grd
--     }

testLevel :: Level
testLevel = f 15 Map.empty
  where
    f 0 m =  Map.insert 0 (standardColumn 0) m
    f x m = f (x-1) (Map.insert x (standardColumn x) m)