module Controller.LevelUpdate where

import Model.Basic
import Model.Player
import Model.Enemy
import Model.Item
import Model.Level
import Model.Model
import Debug.Trace
import Controller.Physics
-- import Graphics.Gloss

import qualified Data.Map as Map

-- Takes care of the movement through the stage in the form of a sliding window
levelUpdate :: Float -> GameState -> GameState
levelUpdate secs g
  -- If the player moves forward and a blocklength is traversed
  | any forward (players g) && blockForward =
    g {
      slidingWindow = windowSlide $ slideBlocksLeft (head (players g)) secs (slidingWindow g)
      ,enemies = slideEnemiesLeft (head (players g)) secs (enemies g)
      ,items = slideItemsLeft (head (players g)) secs (items g)
      ,level = Map.drop 1 $ slideBlocksLeft (head (players g)) secs (level g)
      ,xOffset = fst (getVel (head (players g))) * secs + xOffset g
      ,levelKey = (round (xOffset g) `div` round (16*(entityScale g))) + 17
      ,oldLevelKey = levelKey g
      ,windowShifted = False
      }
  -- If the player moves forward otherwise
  | any forward (players g) =
    g {
      slidingWindow = slideBlocksLeft (head (players g)) secs (slidingWindow g)
      ,enemies = slideEnemiesLeft (head (players g)) secs (enemies g)
      ,items = slideItemsLeft (head (players g)) secs (items g)
      ,level = slideBlocksLeft (head (players g)) secs (level g)
      ,xOffset = fst (getVel (head (players g))) * secs + xOffset g
      ,levelKey = (round (xOffset g) `div` round (16*(entityScale g))) + 17
      }
  | otherwise = g
  where
    blockForward = oldLevelKey g < levelKey g
    xThresHold = fromIntegral (fst res) * 0.12 * windowScale g
    forward :: Player -> Bool
    forward pl = fst (getPos pl) > xThresHold
    lvlkey = levelKey g
    lvl = level g
    windowSlide :: Level -> Level
    windowSlide l = Map.deleteMin $ Map.insert lvlkey (lvl Map.! lvlkey) l

-- Move all blocks left in the world
slideBlocksLeft :: Player -> Float -> Level -> Level
slideBlocksLeft pl secs = Map.foldrWithKey f Map.empty
  where
    f k c = Map.insert k (moveBy (- (fst (getVel pl) * secs),0) c)

-- Move all enemies left in the world
slideEnemiesLeft :: Player -> Float -> [Enemy] -> [Enemy]
slideEnemiesLeft pl secs = foldr f []
  where
    f c ac = moveBy (- (fst (getVel pl) * secs),0) c : ac 

-- Move all items left in the world
slideItemsLeft :: Player -> Float -> [Item] -> [Item]
slideItemsLeft pl secs = foldr f []
  where
    f c ac = moveBy (- (fst (getVel pl) * secs),0) c : ac 