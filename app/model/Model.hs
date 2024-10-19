module Model.Model where

import Model.Basic
import Model.Players
import Model.Enemies
import Model.Platforms
import Model.Blocks
import Model.Items

import Graphics.Gloss.Interface.IO.Game (SpecialKey)

data HasWon     = WON   | LOST      | PLAYING
    deriving(Show,Eq)

data GameState = GameState 
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
    } deriving (Show,Eq)

initialState :: GameState
initialState = GameState
    {   lives = 10
    ,   score = 0
    ,   time = 0.0
    ,   status = PLAYING
    ,   players = [mario]
    ,   enemies = [goomba,goomba2]
    ,   items = []
    ,   blocks = [brick,qblock,empblock]
    -- ,   platforms = []
    ,   platforms = [stair,stair2,pipe1,pipe2,pipe3,pipe4] ++ makeFloor --TODO: replace with mapped column list
    ,   pressedKeys = []
    }