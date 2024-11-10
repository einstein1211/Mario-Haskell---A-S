{-# LANGUAGE InstanceSigs #-}
module Model.Level where

import Model.Basic
import Model.Player
import Model.Enemy
import Model.Item
import Model.Block
import Model.Platform
import View.Scaling
import Graphics.Gloss
import Debug.Trace

import qualified Data.Map as Map

data Spawn = MkPlSpawn Player | MkEnSpawn Enemy | MkItSpawn Item | NoSpawn
    deriving (Eq)
data Chunk = MkBlkChunk Block | MkPltChunk Platform | NoChunk
    deriving (Eq)
-- type TileNumber = Int
data Tile = MkTile Spawn Chunk
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
    show (MkTile NoSpawn NoChunk) = "()"
    show (MkTile spawn chunk) = "("++show spawn++","++show chunk++") "
instance Show Column where
    show (MkColumn []) = "\n"
    show (MkColumn (t:ts)) = show t ++ show (MkColumn ts)

instance PhysicsFunctions Column where
    moveBy :: (Float,Float) -> Column -> Column
    moveBy offset (MkColumn tiles) = MkColumn $ map (moveBy offset) tiles

instance PhysicsFunctions Tile where
    moveBy :: (Float,Float) -> Tile -> Tile
    moveBy offset t@(MkTile spawn chunk) = MkTile (moveBy offset spawn) (moveBy offset chunk)

instance PhysicsFunctions Chunk where
    getPos :: Chunk -> Point
    getPos (MkBlkChunk b) = getPos b
    getPos (MkPltChunk p) = getPos p
    getPos _ = (0,0)
    moveBy offset (MkBlkChunk b) = MkBlkChunk $ moveBy offset b
    moveBy offset (MkPltChunk p) = MkPltChunk $ moveBy offset p
    moveBy _ c = c

instance PhysicsFunctions Spawn where
    moveBy offset (MkEnSpawn e) = MkEnSpawn $ moveBy offset e
    moveBy offset s = s

class ColumnFunctions a where
    addToColumn :: a -> Column -> Column
    getEntries :: Column -> [a]
    -- deleteEntries :: a -> Column -> Column

instance ColumnFunctions Tile where
    addToColumn :: Tile -> Column -> Column
    addToColumn (MkTile _ (MkBlkChunk b)) c = addToColumn b c
    addToColumn (MkTile _ (MkPltChunk p)) c = addToColumn p c
    -- addToColumn (MkTile (MkPlSpawn p) _)  c = addToColumn p c
    addToColumn (MkTile (MkEnSpawn e) _)  c = addToColumn e c
    -- addToColumn (MkTile (MkItSpawn i) _)  c = addToColumn i c
    addToColumn _ c = c

instance ColumnFunctions Int where
    addToColumn :: Int -> Column -> Column
    addToColumn i (MkColumn tiles) = MkColumn (take i tiles ++ emptyTile : drop (i+1) tiles)

instance ColumnFunctions Block where
    addToColumn :: Block -> Column -> Column
    addToColumn b@MkBlock {bPlatform = MkPlatform _ _ (_,y)} (MkColumn tiles)  = MkColumn (filter (bf y) tiles ++ t : filter (af y) tiles)
        where
            bf ypos' (MkTile _ chunk) = snd (getPos chunk) < ypos'
            af ypos' (MkTile _ chunk) = snd (getPos chunk) > ypos'
            t = MkTile NoSpawn (MkBlkChunk b)
    getEntries :: Column -> [Block]
    getEntries (MkColumn tiles) = foldr f [] tiles
        where
            f (MkTile _ (MkBlkChunk b)) ac = b:ac
            f _ ac = ac

instance ColumnFunctions Platform where
    addToColumn :: Platform -> Column -> Column
    addToColumn p@MkPlatform {pfPos = (_,y)} (MkColumn tiles)  = MkColumn (filter (bf y) tiles ++ t : filter (af y) tiles)
        where
            bf ypos' (MkTile _ chunk) = snd (getPos chunk) < ypos'
            af ypos' (MkTile _ chunk) = snd (getPos chunk) > ypos'
            t = MkTile NoSpawn (MkPltChunk p)
    getEntries :: Column -> [Platform]
    getEntries (MkColumn tiles) = foldr f [] tiles
        where
            f (MkTile _ (MkPltChunk p)) ac = p:ac
            f _ ac = ac

instance ColumnFunctions Enemy where
  addToColumn :: Enemy -> Column -> Column
  addToColumn e (MkColumn tiles) = MkColumn (filter (bf y) tiles ++ t : filter (af y) tiles)
    where
      y = snd (getPos e)
      bf ypos' (MkTile _ chunk) = snd (getPos chunk) < ypos'
      af ypos' (MkTile _ chunk) = snd (getPos chunk) > ypos'
      t = MkTile (MkEnSpawn e) NoChunk
  getEntries :: Column -> [Enemy]
  getEntries (MkColumn tiles) = foldr f [] tiles
    where
      f (MkTile (MkEnSpawn e) _) ac = e:ac
      f _ ac = ac

instance ColumnFunctions Item where
  getEntries :: Column -> [Item]
  getEntries (MkColumn tiles) = foldr f [] tiles
    where
      f (MkTile (MkItSpawn i) _) ac = i:ac
      f _ ac = ac

deleteEntries :: Entry -> Column -> Column
deleteEntries entry (MkColumn tiles) = MkColumn $ foldr f [] tiles
    where
        f = case entry of
            PlayerEntry  -> plf
            EnemyEntry   -> enf
            ItemEntry    -> itf
            -- Block   -> blf
            -- Platform-> pltf
        plf (MkTile (MkPlSpawn _) chunk) ac = MkTile NoSpawn chunk:ac
        plf c ac = c:ac
        enf (MkTile (MkEnSpawn _) chunk) ac = MkTile NoSpawn chunk:ac
        enf c ac = c:ac
        itf (MkTile (MkItSpawn _) chunk) ac = MkTile NoSpawn chunk:ac
        itf c ac = c:ac

emptyTile :: Tile
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
    addDirt ac c = addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform DIRT platformHB (grid c)))) ac
    grid x = makeGridPos (0,fromIntegral x) 4

standardColumn :: ColumnNumber -> Column
standardColumn cn =
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform DIRT platformHB (makeGridPos (cn,10) startScaling))))
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform DIRT platformHB (makeGridPos (cn,11) startScaling)))) emptyColumn

goombaColumn :: ColumnNumber -> Column
goombaColumn cn =
  addToColumn (MkTile (MkEnSpawn (makeGoomba (makeGridPos (cn,9) startScaling))) NoChunk) (standardColumn cn)

goombaColumn2 :: ColumnNumber -> Column
goombaColumn2 cn =
  addToColumn (MkTile (MkEnSpawn (makeGoomba (makeGridPos (cn,1) startScaling))) NoChunk) $
  addToColumn (MkTile NoSpawn (MkBlkChunk (MkBlock BRICK (MkPlatform BLOCK (MkHB 16 16) (makeGridPos (cn,2) startScaling)) ALIVE NOITEM False))) (standardColumn cn)

goombaColumn3 :: ColumnNumber -> Column
goombaColumn3 cn =
  addToColumn (MkTile (MkEnSpawn (makeGoomba (makeGridPos (cn,9) startScaling))) NoChunk) $
  addToColumn (MkTile NoSpawn (MkBlkChunk (MkBlock BRICK (MkPlatform BLOCK (MkHB 16 16) (makeGridPos (cn,2) startScaling)) ALIVE NOITEM False))) (standardColumn cn)

koopaColumn :: ColumnNumber -> Column
koopaColumn cn =
  addToColumn (MkTile (MkEnSpawn (makeKoopa (makeGridPos (cn,9) startScaling))) NoChunk) (standardColumn cn)

pipeColumnL :: ColumnNumber -> Column
pipeColumnL cn =
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPETL platformHB (makeGridPos (cn,8) startScaling))))
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPEL platformHB (makeGridPos (cn,9) startScaling)))) (standardColumn cn)

pipeColumnL2 :: ColumnNumber -> Column
pipeColumnL2 cn =
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPETL platformHB (makeGridPos (cn,7) startScaling))))
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPEL platformHB (makeGridPos (cn,8) startScaling))))
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPEL platformHB (makeGridPos (cn,9) startScaling)))) (standardColumn cn)

pipeColumnL3 :: ColumnNumber -> Column
pipeColumnL3 cn =
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPETL platformHB (makeGridPos (cn,6) startScaling))))
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPEL platformHB (makeGridPos (cn,7) startScaling))))
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPEL platformHB (makeGridPos (cn,8) startScaling))))
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPEL platformHB (makeGridPos (cn,9) startScaling)))) (standardColumn cn)

pipeColumnR :: ColumnNumber -> Column
pipeColumnR cn =
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPETR platformHB  (makeGridPos (cn,8) startScaling))))
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPER platformHB (makeGridPos (cn,9) startScaling)))) (standardColumn cn)

pipeColumnR2 :: ColumnNumber -> Column
pipeColumnR2 cn =
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPETR platformHB  (makeGridPos (cn,7) startScaling))))
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPER platformHB (makeGridPos (cn,8) startScaling))))
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPER platformHB (makeGridPos (cn,9) startScaling)))) (standardColumn cn)

pipeColumnR3 :: ColumnNumber -> Column
pipeColumnR3 cn =
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPETR platformHB (makeGridPos (cn,6) startScaling))))
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPER platformHB (makeGridPos (cn,7) startScaling))))
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPER platformHB (makeGridPos (cn,8) startScaling))))
  $ addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform PIPER platformHB (makeGridPos (cn,9) startScaling)))) (standardColumn cn)

mushroomColumn :: ColumnNumber -> Column
mushroomColumn cn =
   addToColumn (MkTile NoSpawn (MkBlkChunk (MkBlock QBLOCK (MkPlatform BLOCK (MkHB 16 16) (makeGridPos (cn,6) startScaling)) ALIVE (makeMushroom (makeGridPos (cn,1) startScaling)) False))) (standardColumn cn)

qColumn :: ColumnNumber -> Column
qColumn cn =
   addToColumn (MkTile NoSpawn (MkBlkChunk (MkBlock QBLOCK (MkPlatform BLOCK (MkHB 16 16) (makeGridPos (cn,6) startScaling)) ALIVE (makeCoin (makeGridPos (cn,1) startScaling)) False))) (standardColumn cn)

qColumn2 :: ColumnNumber -> Column
qColumn2 cn =
    addToColumn (MkTile NoSpawn (MkBlkChunk (MkBlock QBLOCK (MkPlatform BLOCK (MkHB 16 16) (makeGridPos (cn,2) startScaling)) ALIVE (makeCoin (makeGridPos (cn,1) startScaling)) False))) $
    addToColumn (MkTile NoSpawn (MkBlkChunk (MkBlock BRICK (MkPlatform BLOCK (MkHB 16 16) (makeGridPos (cn,6) startScaling)) ALIVE NOITEM False))) (standardColumn cn)

qColumn3 :: ColumnNumber -> Column
qColumn3 cn =
    addToColumn (MkTile NoSpawn (MkBlkChunk (MkBlock QBLOCK (MkPlatform BLOCK (MkHB 16 16) (makeGridPos (cn,2) startScaling)) ALIVE (makeMushroom (makeGridPos (cn,1) startScaling)) False))) $
    addToColumn (MkTile NoSpawn (MkBlkChunk (MkBlock QBLOCK (MkPlatform BLOCK (MkHB 16 16) (makeGridPos (cn,6) startScaling)) ALIVE (makeCoin (makeGridPos (cn,1) startScaling)) False))) (standardColumn cn)

brickColumn :: ColumnNumber -> Column
brickColumn cn =
    addToColumn (MkTile NoSpawn (MkBlkChunk (MkBlock BRICK (MkPlatform BLOCK (MkHB 16 16) (makeGridPos (cn,6) startScaling)) ALIVE NOITEM False))) (standardColumn cn)

brickColumn2 :: ColumnNumber -> Column
brickColumn2 cn =
    addToColumn (MkTile (MkEnSpawn (makeGoomba (makeGridPos (cn,9) startScaling))) NoChunk) $
    addToColumn (MkTile NoSpawn (MkBlkChunk (MkBlock QBLOCK (MkPlatform BLOCK (MkHB 16 16) (makeGridPos (cn,2) startScaling)) ALIVE (makeCoin (makeGridPos (cn,1) startScaling)) False))) $
    addToColumn (MkTile NoSpawn (MkBlkChunk (MkBlock BRICK (MkPlatform BLOCK (MkHB 16 16) (makeGridPos (cn,6) startScaling)) ALIVE NOITEM False))) (standardColumn cn)

brickColumn3 :: ColumnNumber -> Column
brickColumn3 cn =
    addToColumn (MkTile NoSpawn (MkBlkChunk (MkBlock BRICK (MkPlatform BLOCK (MkHB 16 16) (makeGridPos (cn,2) startScaling)) ALIVE NOITEM False))) (standardColumn cn)

brickColumn4 :: ColumnNumber -> Column
brickColumn4 cn =
    addToColumn (MkTile NoSpawn (MkBlkChunk (MkBlock BRICK (MkPlatform BLOCK (MkHB 16 16) (makeGridPos (cn,2) startScaling)) ALIVE NOITEM False))) emptyColumn

hidBlockColumn :: ColumnNumber -> Column
hidBlockColumn cn =
    addToColumn (MkTile NoSpawn (MkBlkChunk (MkBlock HIDDENBLOCK (MkPlatform BLOCK (MkHB 16 16) (makeGridPos (cn,5) startScaling)) ALIVE (makeMushroom (makeGridPos (cn,1) startScaling)) False))) (standardColumn cn)

stairColumn1 :: ColumnNumber -> Column
stairColumn1 cn =
    addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,9) startScaling)))) (standardColumn cn)

stairColumn2 :: ColumnNumber -> Column
stairColumn2 cn =
    addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,8) startScaling)))) $
    addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,9) startScaling)))) (standardColumn cn)

stairColumn3 :: ColumnNumber -> Column
stairColumn3 cn =
    addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,7) startScaling)))) $
    addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,8) startScaling)))) $
    addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,9) startScaling)))) (standardColumn cn)

stairColumn4 :: ColumnNumber -> Column
stairColumn4 cn =
    addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,6) startScaling)))) $
    addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,7) startScaling)))) $
    addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,8) startScaling)))) $
    addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,9) startScaling)))) (standardColumn cn)

stairColumn5 :: ColumnNumber -> Column
stairColumn5 cn =
    addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,5) startScaling)))) $
    addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,6) startScaling)))) $
    addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,7) startScaling)))) $
    addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,8) startScaling)))) $
    addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,9) startScaling)))) (standardColumn cn)

stairColumn6 :: ColumnNumber -> Column
stairColumn6 cn =
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,4) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,5) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,6) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,7) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,8) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,9) startScaling)))) (standardColumn cn)

stairColumn7 :: ColumnNumber -> Column
stairColumn7 cn =
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,3) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,4) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,5) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,6) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,7) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,8) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,9) startScaling)))) (standardColumn cn)

stairColumn8 :: ColumnNumber -> Column
stairColumn8 cn =
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,2) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,3) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,4) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,5) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,6) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,7) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,8) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,9) startScaling)))) (standardColumn cn)

flagpoleColumn :: ColumnNumber -> Column
flagpoleColumn cn =
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform FLAGTOP platformHB (makeGridPos (cn,2) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform FLAGPOLE platformHB (makeGridPos (cn,3) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform FLAGPOLE platformHB (makeGridPos (cn,4) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform FLAGPOLE platformHB (makeGridPos (cn,5) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform FLAGPOLE platformHB (makeGridPos (cn,6) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform FLAGPOLE platformHB (makeGridPos (cn,7) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform FLAGPOLE platformHB (makeGridPos (cn,8) startScaling)))) $
  addToColumn (MkTile NoSpawn (MkPltChunk (MkPlatform STAIR platformHB (makeGridPos (cn,9) startScaling)))) (standardColumn cn)

testLevel :: Level
testLevel = f 255 Map.empty
  where
    f x m
      | x <= -2 = Map.insert (-2) (standardColumn (-2)) m
      | x == 5 = f (x-1) (Map.insert x (flagpoleColumn x) m)
      -- | x == 10 = f (x-1) (Map.insert x (koopaColumn x) m)
      | x == 17 = f (x-1) (Map.insert x (qColumn x) m)
      | x == 21 = f (x-1) (Map.insert x (brickColumn x) m)
      | x == 22 = f (x-1) (Map.insert x (mushroomColumn x) m)
      | x == 23 = f (x-1) (Map.insert x (brickColumn2 x) m)
      | x == 24 = f (x-1) (Map.insert x (qColumn x) m)
      | x == 25 = f (x-1) (Map.insert x (brickColumn x) m)
      | x == 29 = f (x-1) (Map.insert x (pipeColumnL x) m)
      | x == 30 = f (x-1) (Map.insert x (pipeColumnR x) m)
      | x == 39 = f (x-1) (Map.insert x (pipeColumnL2 x) m)
      | x == 40 = f (x-1) (Map.insert x (pipeColumnR2 x) m)
      | x == 41 = f (x-1) (Map.insert x (goombaColumn x) m)
      | x == 47 = f (x-1) (Map.insert x (pipeColumnL3 x) m)
      | x == 48 = f (x-1) (Map.insert x (pipeColumnR3 x) m)
      | x == 52 = f (x-1) (Map.insert x (goombaColumn x) m)
      | x == 53 = f (x-1) (Map.insert x (goombaColumn x) m)
      | x == 58 = f (x-1) (Map.insert x (pipeColumnL3 x) m)
      | x == 59 = f (x-1) (Map.insert x (pipeColumnR3 x) m)
      | x == 65 = f (x-1) (Map.insert x (hidBlockColumn x) m)
      | x == 70 = f (x-1) (Map.insert x emptyColumn m)
      | x == 71 = f (x-1) (Map.insert x emptyColumn m)
      | x == 78 = f (x-1) (Map.insert x (brickColumn x) m)
      | x == 79 = f (x-1) (Map.insert x (mushroomColumn x) m)
      | x == 80 = f (x-1) (Map.insert x (brickColumn x) m)
      | x == 81 = f (x-1) (Map.insert x (goombaColumn2 x) m)
      | x == 82 = f (x-1) (Map.insert x (brickColumn3 x) m)
      | x == 83 = f (x-1) (Map.insert x (goombaColumn2 x) m)
      | x > 83 && x < 88 = f (x-1) (Map.insert x (brickColumn3 x) m)
      | x == 88 || x == 89 = f (x-1) (Map.insert x (brickColumn4 x) m)
      | x == 90 = f (x-1) (Map.insert x emptyColumn m)
      | x > 92 && x < 96 = f (x-1) (Map.insert x (brickColumn3 x) m)
      | x == 96 = f (x-1) (Map.insert x (qColumn2 x) m)
      | x == 99 = f (x-1) (Map.insert x (goombaColumn x) m)
      | x == 101 = f (x-1) (Map.insert x (goombaColumn x) m)
      | x == 102 = f (x-1) (Map.insert x (brickColumn x) m)
      | x == 103 = f (x-1) (Map.insert x (brickColumn x) m)
      | x == 108 = f (x-1) (Map.insert x (qColumn x) m)
      | x == 109 = f (x-1) (Map.insert x (koopaColumn x) m)
      | x == 111 = f (x-1) (Map.insert x (qColumn3 x) m)
      | x == 114 = f (x-1) (Map.insert x (qColumn x) m)
      | x == 116 = f (x-1) (Map.insert x (goombaColumn x) m)
      | x == 118 = f (x-1) (Map.insert x (goombaColumn x) m)
      | x == 120 = f (x-1) (Map.insert x (brickColumn x) m)
      | x > 122 && x < 127 = f (x-1) (Map.insert x (brickColumn3 x) m)
      | x == 127 = f (x-1) (Map.insert x (goombaColumn x) m)
      | x == 129 = f (x-1) (Map.insert x (goombaColumn x) m)
      | x == 133 || x == 134 = f (x-1) (Map.insert x (brickColumn2 x) m)
      | x == 135 = f (x-1) (Map.insert x (brickColumn3 x) m)
      | x == 138 = f (x-1) (Map.insert x (stairColumn1 x) m)
      | x == 139 = f (x-1) (Map.insert x (stairColumn2 x) m)
      | x == 140 = f (x-1) (Map.insert x (stairColumn3 x) m)
      | x == 141 = f (x-1) (Map.insert x (stairColumn4 x) m)
      | x == 144 = f (x-1) (Map.insert x (stairColumn4 x) m)
      | x == 145 = f (x-1) (Map.insert x (stairColumn3 x) m)
      | x == 146 = f (x-1) (Map.insert x (stairColumn2 x) m)
      | x == 147 = f (x-1) (Map.insert x (stairColumn1 x) m)
      | x == 152 = f (x-1) (Map.insert x (stairColumn1 x) m)
      | x == 153 = f (x-1) (Map.insert x (stairColumn2 x) m)
      | x == 154 = f (x-1) (Map.insert x (stairColumn3 x) m)
      | x == 155 = f (x-1) (Map.insert x (stairColumn4 x) m)
      | x == 156 = f (x-1) (Map.insert x (stairColumn4 x) m)
      | x == 157 || x == 158 = f (x-1) (Map.insert x emptyColumn m)
      | x == 159 = f (x-1) (Map.insert x (stairColumn4 x) m)
      | x == 160 = f (x-1) (Map.insert x (stairColumn3 x) m)
      | x == 161 = f (x-1) (Map.insert x (stairColumn2 x) m)
      | x == 162 = f (x-1) (Map.insert x (stairColumn1 x) m)
      | x == 167 = f (x-1) (Map.insert x (pipeColumnL x) m)
      | x == 168 = f (x-1) (Map.insert x (pipeColumnR x) m)
      | x == 172 || x == 173 = f (x-1) (Map.insert x (brickColumn x) m)
      | x == 174 = f (x-1) (Map.insert x (qColumn x) m)
      | x == 175 = f (x-1) (Map.insert x (brickColumn x) m)
      | x == 178 = f (x-1) (Map.insert x (goombaColumn x) m)
      | x == 180 = f (x-1) (Map.insert x (goombaColumn x) m)
      | x == 183 = f (x-1) (Map.insert x (pipeColumnL x) m)
      | x == 184 = f (x-1) (Map.insert x (pipeColumnR x) m)
      | x == 185 = f (x-1) (Map.insert x (stairColumn1 x) m)
      | x == 186 = f (x-1) (Map.insert x (stairColumn2 x) m)
      | x == 187 = f (x-1) (Map.insert x (stairColumn3 x) m)
      | x == 188 = f (x-1) (Map.insert x (stairColumn4 x) m)
      | x == 189 = f (x-1) (Map.insert x (stairColumn5 x) m)
      | x == 190 = f (x-1) (Map.insert x (stairColumn6 x) m)
      | x == 191 = f (x-1) (Map.insert x (stairColumn7 x) m)
      | x == 192 = f (x-1) (Map.insert x (stairColumn8 x) m)
      | x == 193 = f (x-1) (Map.insert x (stairColumn8 x) m)
      | x == 202 = f (x-1) (Map.insert x (flagpoleColumn x) m)
      | otherwise = f (x-1) (Map.insert x (standardColumn x) m)

-- initialWindow :: [Column]
-- initialWindow = Map.foldr (:) [] (Map.take 22 testLevel)

initialWindow :: Level
initialWindow = Map.take 20 testLevel