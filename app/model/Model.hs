module Model.Model where

import Model.Player
import Model.Enemy
import Model.Item
import Model.Block
import Model.Platform

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
    ,   blocks :: [Block]
    ,   platforms :: [Platform]
    ,   pressedKeys :: [SpecialKey]
    ,   debugMode :: Bool
    } deriving (Show,Eq)

initialState :: GameState
initialState = MkGameState
    {   lives = 10
    ,   score = 0
    ,   time = 0.0
    ,   status = PLAYING
    ,   players = [mario]
    ,   enemies = [goomba,goomba2]
    ,   items = [coin]
    ,   blocks = [brick,qblock,empblock,hidblock]
    -- ,   platforms = []
    ,   platforms = [stair,stair2,pipe1,pipe2,pipe3,pipe4] ++ makeFloor --TODO: replace with mapped column list
    ,   pressedKeys = []
    ,   debugMode = False
    }
