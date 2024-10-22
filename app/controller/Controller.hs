module Controller.Controller where

import Model.Model
import Model.Player
import Controller.Physics
import Controller.Entity
import Graphics.Gloss.Interface.IO.Game

-- import System.Exit

directKey :: [SpecialKey]
directKey = [KeyDown,KeyUp,KeyLeft,KeyRight,KeySpace,KeyShiftL]

step :: Float -> GameState -> IO GameState
step secs gstate = do
  -- print (players gstate)
  -- print (map pMovement (players gstate))
  return $ entityInteractions secs $ applyPhysics secs $ entityUpdate gstate {time = time gstate + secs}

-- | Handle user input
input :: Event -> GameState -> IO GameState
-- input e g =
--   do
--     putStrLn $ show e
--     return g
input e gstate = return (inputKey e gstate)
-- input _ = return

inputKey :: Event -> GameState -> GameState
inputKey e@(EventKey (SpecialKey key) state _ _) gstate
  | dkey = playerMove e gstate
  | otherwise =
    case key of
      KeyEsc   -> undefined--exitSuccess
      KeyCtrlL -> if state == Down then gstate {debugMode = not (debugMode gstate)} else gstate
      _ -> error ":("
      where
        dkey = key `elem` directKey
inputKey _ gstate = gstate -- Otherwise keep the same

playerMove :: Event -> GameState -> GameState
playerMove (EventKey (SpecialKey key) state _ _) g
  | state == Down = g {pressedKeys = key:pks}
  | otherwise     = g {pressedKeys = filter (/=key) pks}
    where
      pks = pressedKeys g

