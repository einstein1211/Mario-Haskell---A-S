module Model.Model where

import Model.Basic
import Model.Player
import Model.Enemy
import Model.Item
import Model.Block
import Model.Platform
import Model.Level
import View.Scaling

import Graphics.Gloss.Interface.IO.Game (SpecialKey)

data HasWon     = WON   | LOST      | PLAYING
    deriving(Show,Eq)

data GameMode = StartMenu | Playing deriving (Eq, Show) 

data GameState = MkGameState 
    {   lives :: Int
    ,   score :: Int
    ,   time :: Float --Maybe could be Int
    ,   status :: HasWon
    ,   players :: [Player]
    ,   enemies :: [Enemy]
    ,   items :: [Item]
    -- ,   blocks :: [Block]
    ,   platforms :: [Platform]
    ,   level :: Level
    ,   levelKey :: Int
    ,   oldLevelKey :: Int
    -- ,   slidingWindow :: [Column]
    ,   slidingWindow :: Level
    ,   xOffset :: Float
    -- ,   column :: Column
    ,   pressedKeys :: [SpecialKey]
    ,   debugMode :: Bool
    ,   windowRes :: Resolution
    ,   windowScale :: Scaling
    ,   entityScale :: Scaling
    ,   isScaled :: Bool
    ,   reScaled :: Bool
    ,   windowShifted :: Bool
    ,   isPaused :: Bool
    ,   mode :: GameMode 
    } deriving (Show,Eq)

initialState :: GameState
initialState = MkGameState
    {   lives = 10
    ,   score = 0
    ,   time = 0.0
    ,   status = PLAYING
    ,   players = [mario]
    ,   enemies = []
    -- ,   enemies = [goomba,goomba2,koopa]
    ,   items = []
    -- ,   items = [coin, mushroom]
    -- ,   blocks = []
    -- ,   blocks = [brick,qblock,empblock,hidblock]
    ,   platforms = []
    -- ,   platforms = [stair,stair2,pipe1,pipe2,pipe3,pipe4] ++ makeFloor --TODO: replace with mapped column list
    ,   level = testLevel
    -- ,   levelKey = 1
    ,   levelKey = 17
    ,   oldLevelKey = 17
    ,   slidingWindow = initialWindow
    ,   xOffset = 0
    -- ,   column = testColumn
    ,   pressedKeys = []
    ,   debugMode = False
    ,   windowRes = res
    ,   windowScale = 1
    ,   entityScale = 4
    ,   isScaled = False
    ,   reScaled = False
    ,   windowShifted = False
    ,   isPaused = False
    ,   mode = StartMenu
    }
