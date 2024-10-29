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
    trace (show (blocks g))
    trace (show (platforms g))
    g {
      -- slidingWindow = slideblocks $ slidingWindow g
      -- slidingWindow = slideblocks $ (Map.drop (lvlkey) (Map.take (lvlkey+1) lvl)) <> Map.drop 1 (slidingWindow g) 
      -- slidingWindow = slideblocks $ Map.insert (lvlkey+1) nextColumn $  Map.drop 1 (slidingWindow g)  --( <$ Map.drop (lvlkey) (Map.take (lvlkey+1)lvl))
      slidingWindow = slideblocks $ Map.take 20 $ Map.drop (levelKey g) lvl
      ,players = map (moveBy (-(32*windowScale g),0)) (players g)
      ,levelKey = lvlkey + 1
      ,windowShifted = False
      }
  | otherwise = g
  where
    -- nextColumn :: Column 
    -- nextColumn = changeColumnIndex lvlkey $ (lvl) Map.! (lvlkey)

    -- changeColumnIndex :: Int -> Column -> Column 
    -- changeColumnIndex n (MkColumn a ) = MkColumn (fmap (\(n2,t) -> changeGridIndex (MkGrid n n2) t) $ zip [0..] a ) 
    -- nextcol = lvl Map.!? lvlkey
    -- input =
    --   case nextcol of
    --     Nothing -> error "empty column"
    --     Just x -> x
    forward :: Player -> Bool 
    forward pl = fst (getPos pl) > fst (gridPos (MkGrid 9 0) (windowScale g))
    lvlkey = levelKey g
    lvl = level g
    slideblocks :: Level -> Level
    slideblocks l = Map.foldrWithKey f Map.empty l
      where f k c ac = Map.insert k (slideblocks' c) ac
    -- slideblocks [] = []
    -- slideblocks ((MkColumn tiles):cs) =  MkColumn (slideblocks' tiles) : slideblocks cs
    -- slideblocks' :: [Tile] -> [Tile]
    slideblocks' :: Column -> Column
    slideblocks' (MkColumn tiles) = MkColumn $ map f tiles
    -- slideblocks' (MkColumn []) = MkColumn []
    -- slideblocks' (MkColumn (t:ts)) = MkColumn $ changeGridIndex (MkGrid (x-1) y) t : MkColumn (slideblocks' ts)
      where
        f t = changeGridIndex (MkGrid (x-lvlkey) y) t
          where
            (MkGrid x y) = getGridIndex t

