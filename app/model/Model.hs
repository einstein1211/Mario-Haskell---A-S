module Model.Model where

import Model.Basic
import Model.Player
import Model.Enemy
import Model.Item
import Model.Platform
import Model.Level

import Graphics.Gloss.Interface.IO.Game (SpecialKey)

-- Gamemode datatype to see if the game is still going
data GameMode = StartMenu | Playing | Exiting deriving (Eq, Show) 

-- GameState datatype that holds all important global info
data GameState = MkGameState 
    {   lives :: Int
    ,   score :: Int
    ,   time :: Float 
    ,   players :: [Player]
    ,   enemies :: [Enemy]
    ,   items :: [Item]
    ,   platforms :: [Platform]
    ,   level :: Level
    ,   levelKey :: Int
    ,   oldLevelKey :: Int
    ,   slidingWindow :: Level
    ,   xOffset :: Float
    ,   pressedKeys :: [SpecialKey]
    ,   debugMode :: Bool
    ,   windowRes :: Resolution
    ,   windowScale :: Scaling
    ,   entityScale :: Scaling
    -- ,   reScaled :: Bool
    ,   windowShifted :: Bool
    ,   isPaused :: Bool
    ,   mode :: GameMode 
    } deriving (Show,Eq)

-- Initial gamestate at start of Game
initialState :: GameState
initialState = MkGameState
    {   lives = 3
    ,   score = 0
    ,   time = 0.0
    ,   players = [mario]
    ,   enemies = []
    ,   items = []
    ,   platforms = []
    ,   level = level1
    ,   levelKey = 17
    ,   oldLevelKey = 17
    ,   slidingWindow = initialWindow
    ,   xOffset = 0
    ,   pressedKeys = []
    ,   debugMode = False
    ,   windowRes = res
    ,   windowScale = 1
    ,   entityScale = 4
    -- ,   reScaled = False
    ,   windowShifted = False
    ,   isPaused = False
    ,   mode = StartMenu
    }
