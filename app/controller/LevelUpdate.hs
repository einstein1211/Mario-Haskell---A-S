module Controller.LevelUpdate where

import Model.Basic
import Model.Player
import Model.Level
import Model.Model
import Debug.Trace
import Controller.Physics
-- import Graphics.Gloss

import qualified Data.Map as Map

levelUpdate :: Float -> GameState -> GameState
levelUpdate secs g
  | any forward (players g) && blockForward = -- && windowShifted g =
    -- trace (show (xOffset g))
    trace (show (levelKey g))
    -- trace (show (oldLevelKey g))
    g {
      slidingWindow = windowSlide $ slideBlocksLeft (head (players g)) secs (slidingWindow g) -- $  (slidingWindow g)
      ,level = slideBlocksLeft (head (players g)) secs (level g)
      ,xOffset = fst (getVel (head (players g))) * secs + xOffset g
      -- ,players = map (moveBy (-(32*windowScale g),0)) (players g)
      ,levelKey = (round(xOffset g) `div` round(16*(entityScale g))) + 17
      ,oldLevelKey = levelKey g
      -- ,windowShifted = False
      }
  | any forward (players g) = -- && windowShifted g =
    trace (show (levelKey g))
    g {
      slidingWindow = slideBlocksLeft (head (players g)) secs (slidingWindow g) -- $  (slidingWindow g)
      ,level = slideBlocksLeft (head (players g)) secs (level g)
      ,xOffset = fst (getVel (head (players g))) * secs + xOffset g
      -- ,players = map (moveBy (-(32*windowScale g),0)) (players g)
      ,levelKey = (round(xOffset g) `div` round(16*(entityScale g))) + 17
      -- ,windowShifted = False
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
        
slideBlocksLeft :: Player -> Float -> Level -> Level
slideBlocksLeft pl secs = Map.foldlWithKey f Map.empty
  where
    f ac k c = Map.insert k (moveBy (- (fst (getVel pl) * secs),0) c) ac

