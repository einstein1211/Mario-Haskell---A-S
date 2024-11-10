module Controller.Controller where

import Model.Basic
import Model.Model
import Controller.Physics
import Controller.Entity
import Controller.LevelUpdate
import Graphics.Gloss.Interface.IO.Game
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)
import System.IO (appendFile)

-- Keys used to control the player
directKey :: [SpecialKey]
directKey = [KeyDown,KeyUp,KeyLeft,KeyRight,KeySpace,KeyShiftL]

-- Write the score to a text file
writeScore :: Int -> IO ()
writeScore sc = do
  -- Open the file for appending (create if it doesn't exist)
  appendFile "score.txt" (show sc ++ "\n")

-- Step function, for incrementing game physics with time
step :: Float -> GameState -> IO GameState
step secs gstate
  | mode gstate == StartMenu = return gstate
  | isPaused gstate = return gstate
  | (not (any isAlive (players gstate)) || xOffset gstate >= 12225) && mode gstate == Playing = return gstate {mode = Exiting}
  | mode gstate == Exiting = do
      writeScore (score gstate)
      putStrLn $ "Thanks for playing! Your final score is " ++ show (score gstate)
      threadDelay 3000000
      exitSuccess
  | otherwise = return $ entityInteractions secs $ applyPhysics secs $ levelUpdate secs $ entityUpdate gstate { time = time gstate + secs }

-- Input function, handles input from user
input :: Event -> GameState -> IO GameState
input e gstate = case e of
  -- Start menu actions
  EventKey (Char 's') Down _ _ | mode gstate == StartMenu -> return gstate { mode = Playing }
  EventKey (Char 'q') Down _ _ | mode gstate == StartMenu -> exitSuccess
  EventKey (Char 'p') Down _ _ | mode gstate == Playing   -> return gstate { isPaused = not (isPaused gstate) }
  EventKey (Char 'm') Down _ _ | isPaused gstate -> return gstate { mode = StartMenu, isPaused = False }
  _ | mode gstate == Playing -> return $ inputKey e gstate -- return $ (inputKey e . resizeEvent e) gstate
  _ | mode gstate == Exiting -> return gstate
  _ -> return gstate

-- resizeEvent :: Event -> GameState -> GameState
-- resizeEvent (EventResize (x,y)) g = windowScaling (x,y) g
-- resizeEvent _ g = g

-- windowScaling :: Resolution -> GameState -> GameState
-- windowScaling (x,y) g = g {windowScale = sx, entityScale = sx*4, reScaled = True}
--   where
--     fac  = fromIntegral y / fromIntegral x
--     lead 
--       | fac<0.75  = y `div` 12
--       | otherwise = x `div` 16
--     (sx,sy) = (fromIntegral (lead*16)/1024.0,fromIntegral (lead*12)/768.0)

-- Inputkey function, handles user input in the form of movement keys
inputKey :: Event -> GameState -> GameState
inputKey e@(EventKey (SpecialKey key) state _ _) gstate
  | dkey = playerMove e gstate
  | otherwise =
    case key of
      -- KeyEsc   -> error (show (slidingWindow gstate))--exitSuccess
      KeyCtrlL -> if state == Down then gstate {debugMode = not (debugMode gstate)} else gstate
      _ -> error ":("
      where
        dkey = key `elem` directKey
inputKey _ gstate = gstate -- Otherwise keep the same

-- Playermove function, makes a list of the currently pressed movement keys
playerMove :: Event -> GameState -> GameState
playerMove (EventKey (SpecialKey key) state _ _) g
  | state == Down = g {pressedKeys = key:pks}
  | otherwise     = g {pressedKeys = filter (/=key) pks}
    where
      pks = pressedKeys g


