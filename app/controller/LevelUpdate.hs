module Controller.LevelUpdate where

import Model.Basic
import Model.Player
import Model.Level
import Model.Model
import Debug.Trace
import Controller.Physics
-- import Graphics.Gloss

import qualified Data.Map as Map

levelUpdate :: GameState -> GameState
levelUpdate g
  | any forward (players g) && windowShifted g =
    -- trace (show (blocks g))
    -- trace (show (platforms g))
    -- trace (show (levelKey g))
    -- trace (show maxkey)
    -- trace ("\n")
    g {
      -- slidingWindow = slideblocks $ slidingWindow g
      -- slidingWindow = slideblocks $ (Map.drop (lvlkey) (Map.take (lvlkey+1) lvl)) <> Map.drop 1 (slidingWindow g) 
      -- slidingWindow = slideblocks $ Map.insert lvlkey (lvlkey+1) (level g) $  Map.drop 1 (slidingWindow g)
      slidingWindow = slideBlocksLeft $ Map.deleteMin $ Map.insert lvlkey (lvl Map.! lvlkey) (slidingWindow g)
      -- slidingWindow = slideblocks (levelKey g) $ Map.take 20 $ Map.drop (levelKey g) lvl
      ,players = map (moveBy (-(32*windowScale g),0)) (players g)
      ,levelKey = lvlkey + 1
      ,windowShifted = False
      }
  | otherwise = g
  where
    (maxkey,_) = Map.findMax (slidingWindow g)
    forward :: Player -> Bool 
    forward pl = fst (getPos pl) > fst (gridPos (MkGrid 9 0) (windowScale g))
    lvlkey = levelKey g
    lvl = level g
    slideBlocksLeft :: Level -> Level
    slideBlocksLeft = Map.foldrWithKey f Map.empty
      where
        f k c ac = Map.insert k (changeGridIndex (MkGrid (k-lvlkey+18) y) c) ac
          where
            (MkGrid x y) = getGridIndex c
    -- slideblocks :: Int -> Level -> Level
    -- slideblocks i l = Map.foldrWithKey f Map.empty l
    --   where f k c ac = Map.insert k (slideblocks' i c) ac
    -- slideblocks [] = []
    -- slideblocks ((MkColumn tiles):cs) =  MkColumn (slideblocks' tiles) : slideblocks cs
    -- slideblocks' :: [Tile] -> [Tile]
    -- slideblocks' :: Int -> Column -> Column
    -- slideblocks' i (MkColumn tiles) = MkColumn $ map f tiles
    -- slideblocks' (MkColumn []) = MkColumn []
    -- slideblocks' (MkColumn (t:ts)) = MkColumn $ changeGridIndex (MkGrid (x-1) y) t : MkColumn (slideblocks' ts)
      -- where
      --   f t = changeGridIndex (MkGrid (x-i) y) t
      --     where
      --       (MkGrid x y) = getGridIndex t

