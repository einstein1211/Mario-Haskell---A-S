module Model.Model where

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
    -- ,   slidingWindow :: [Column]
    ,   slidingWindow :: Level
    -- ,   column :: Column
    ,   pressedKeys :: [SpecialKey]
    ,   debugMode :: Bool
    ,   windowRes :: Resolution
    ,   windowScale :: Scaling
    ,   entityScale :: Scaling
    ,   isScaled :: Bool
    ,   reScaled :: Bool
    ,   windowShifted :: Bool
    } deriving (Show,Eq)

initialState :: GameState
initialState = MkGameState
    {   lives = 10
    ,   score = 0
    ,   time = 0.0
    ,   status = PLAYING
    ,   players = [mario]
    ,   enemies = [goomba,goomba2]
    ,   items = []
    -- ,   blocks = []
    -- ,   blocks = [brick,qblock,empblock,hidblock]
    ,   platforms = []
    -- ,   platforms = [stair,stair2,pipe1,pipe2,pipe3,pipe4] ++ makeFloor --TODO: replace with mapped column list
    ,   level = testLevel
    -- ,   levelKey = 1
    ,   levelKey = 18
    ,   slidingWindow = initialWindow
    -- ,   column = testColumn
    ,   pressedKeys = []
    ,   debugMode = False
    ,   windowRes = (1024,768)
    ,   windowScale = 1
    ,   entityScale = 4
    ,   isScaled = False
    ,   reScaled = False
    ,   windowShifted = False
    }
