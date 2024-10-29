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
    deriving (Eq)
data Chunk = MkBlkChunk Block | MkPltChunk Platform | NoChunk
    deriving (Eq)
data Tile = MkTile Spawn Chunk Int
    deriving (Eq)
newtype Column = MkColumn [Tile]
    deriving (Eq)
type ColumnNumber = Int
type Level = Map.Map ColumnNumber Column

instance Show Spawn where
    show (MkPlSpawn p) = show p
    show (MkEnSpawn e) = show e
    show (MkItSpawn i) = show i
    show _ = "_"
instance Show Chunk where
    show (MkBlkChunk b) = show b
    show (MkPltChunk p) = show p
    show _ = "_"
instance Show Tile where
    show (MkTile NoSpawn NoChunk _) = "()"
    show (MkTile spawn chunk i) = "("++show spawn++","++show chunk++") "++show i
instance Show Column where
    show (MkColumn []) = "\n"
    show (MkColumn (t:ts)) = show t ++ show (MkColumn ts)


class ColumnFunctions a where
    addToColumn :: a -> Column -> Column
    getEntries :: Column -> [a]

instance ColumnFunctions Tile where
    addToColumn :: Tile -> Column -> Column
    addToColumn t@(MkTile _ _ i) (MkColumn tiles)  = MkColumn (take i tiles ++ t : drop (i+1) tiles)

instance ColumnFunctions Int where
    addToColumn :: Int -> Column -> Column
    addToColumn i (MkColumn tiles) = MkColumn (take i tiles ++ emptyTile i : drop (i+1) tiles)

instance ColumnFunctions Block where
    addToColumn :: Block -> Column -> Column
    addToColumn b@(MkBlock _ (MkPlatform _ _ (MkGrid _ y)) _ _) (MkColumn tiles)  = MkColumn (take y tiles ++ t : drop (y+1) tiles)
        where
            t = MkTile NoSpawn (MkBlkChunk b) y
    getEntries :: Column -> [Block]
    getEntries (MkColumn tiles) = foldr f [] tiles
        where
            f (MkTile _ (MkBlkChunk b) _) ac = b:ac
            f _ ac = ac

instance ColumnFunctions Platform where
    addToColumn :: Platform -> Column -> Column
    addToColumn p@(MkPlatform _ _ (MkGrid _ y)) (MkColumn tiles)  = MkColumn (take y tiles ++ t : drop (y+1) tiles)
        where
            t = MkTile NoSpawn (MkPltChunk p) y
    getEntries :: Column -> [Platform]
    getEntries (MkColumn tiles) = foldr f [] tiles
        where
            f (MkTile _ (MkPltChunk p) _) ac = p:ac
            f _ ac = ac

instance GridIndexFunctions Column where
    changeGridIndex grd (MkColumn tiles) = MkColumn $ map (changeGridIndex grd) tiles

instance GridIndexFunctions Tile where
    changeGridIndex (MkGrid x y) (MkTile spawn chunk i) = MkTile spawn (changeGridIndex (MkGrid x i) chunk) i
    getGridIndex (MkTile _ chunk _) = getGridIndex chunk

instance GridIndexFunctions Chunk where
    changeGridIndex grd (MkBlkChunk b) = MkBlkChunk (changeGridIndex grd b)
    changeGridIndex grd (MkPltChunk p) = MkPltChunk (changeGridIndex grd p)
    changeGridIndex _ c = c
    getGridIndex (MkBlkChunk b) = getGridIndex b
    getGridIndex (MkPltChunk p) = getGridIndex p
    getGridIndex _ = MkGrid 0 0

emptyTile :: Int -> Tile
emptyTile = MkTile NoSpawn NoChunk

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
        grid x = MkGrid 0 (fromIntegral x)

standardColumn :: ColumnNumber -> Column
standardColumn cn = 
    addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform DIRT platformHB (MkGrid cn 10))) 10) 
    $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform DIRT platformHB (MkGrid cn 11))) 11) emptyColumn

pipeColumnL :: ColumnNumber -> Column
pipeColumnL cn =
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPETL platformHB (MkGrid cn 8))) 8)
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPEL platformHB (MkGrid cn 9))) 9) (standardColumn cn)

pipeColumnL2 :: ColumnNumber -> Column
pipeColumnL2 cn =
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPETL platformHB (MkGrid cn 7))) 7)
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPEL platformHB (MkGrid cn 8))) 8)
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPEL platformHB (MkGrid cn 9))) 9) (standardColumn cn)

pipeColumnL3 :: ColumnNumber -> Column
pipeColumnL3 cn =
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPETL platformHB (MkGrid cn 6))) 6)
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPEL platformHB (MkGrid cn 7))) 7)
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPEL platformHB (MkGrid cn 8))) 8)
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPEL platformHB (MkGrid cn 9))) 9) (standardColumn cn)

pipeColumnR :: ColumnNumber -> Column
pipeColumnR cn =
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPETR platformHB (MkGrid cn 8))) 8)
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPER platformHB (MkGrid cn 9))) 9) (standardColumn cn)

pipeColumnR2 :: ColumnNumber -> Column
pipeColumnR2 cn =
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPETR platformHB (MkGrid cn 7))) 7)
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPER platformHB (MkGrid cn 8))) 8)
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPER platformHB (MkGrid cn 9))) 9) (standardColumn cn)

pipeColumnR3 :: ColumnNumber -> Column
pipeColumnR3 cn =
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPETR platformHB (MkGrid cn 6))) 6)
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPER platformHB (MkGrid cn 7))) 7)
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPER platformHB (MkGrid cn 8))) 8)
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPER platformHB (MkGrid cn 9))) 9) (standardColumn cn)

qColumn :: ColumnNumber -> Column
qColumn cn =
--   addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (MkGrid cn 5))) 5) $
   addToColumn (MkTile NoSpawn (MkBlkChunk (MkBlock QBLOCK (MkPlatform BLOCK (MkHB 16 16) (MkGrid cn 6)) ALIVE NOITEM)) 6) (standardColumn cn)

brickColumn :: ColumnNumber -> Column
brickColumn cn =
    addToColumn (MkTile NoSpawn (MkBlkChunk (MkBlock BRICK (MkPlatform BLOCK (MkHB 16 16) (MkGrid cn 6)) ALIVE NOITEM)) 6) (standardColumn cn)

brickColumn2 :: ColumnNumber -> Column
brickColumn2 cn =
    addToColumn (MkTile NoSpawn (MkBlkChunk (MkBlock QBLOCK (MkPlatform BLOCK (MkHB 16 16) (MkGrid cn 2)) ALIVE NOITEM)) 2) $
    addToColumn (MkTile NoSpawn (MkBlkChunk (MkBlock BRICK (MkPlatform BLOCK (MkHB 16 16) (MkGrid cn 6)) ALIVE NOITEM)) 6) (standardColumn cn)


testLevel :: Level
testLevel = f 255 Map.empty
  where
    f x m
      | x <= -2 = Map.insert (-2) (standardColumn (-2)) m
      | x == 17 = f (17-1) (Map.insert 17 (qColumn 17) m)
      | x == 21 = f (21-1) (Map.insert 21 (brickColumn 21) m)
      | x == 22 = f (22-1) (Map.insert 22 (qColumn 22) m)
      | x == 23 = f (23-1) (Map.insert 23 (brickColumn2 23) m)
      | x == 24 = f (24-1) (Map.insert 24 (qColumn 24) m)
      | x == 25 = f (25-1) (Map.insert 25 (brickColumn 25) m)
      | x == 29 = f (29-1) (Map.insert 29 (pipeColumnL 29) m)
      | x == 30 = f (30-1) (Map.insert 30 (pipeColumnR 30) m)
      | x == 39 = f (39-1) (Map.insert 39 (pipeColumnL2 39) m)
      | x == 40 = f (40-1) (Map.insert 40 (pipeColumnR2 40) m)
      | x == 47 = f (47-1) (Map.insert 47 (pipeColumnL3 47) m)
      | x == 48 = f (48-1) (Map.insert 48 (pipeColumnR3 48) m)
      | x == 58 = f (58-1) (Map.insert 58 (pipeColumnL3 58) m)
      | x == 59 = f (59-1) (Map.insert 59 (pipeColumnR3 59) m)
      | x == 70 = f (70-1) (Map.insert 70 emptyColumn m)
      | x == 71 = f (71-1) (Map.insert 71 emptyColumn m)
      | otherwise = f (x-1) (Map.insert x (standardColumn x) m)

-- initialWindow :: [Column]
-- initialWindow = Map.foldr (:) [] (Map.take 22 testLevel)

initialWindow :: Level
initialWindow = Map.take 20 testLevel