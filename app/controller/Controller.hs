module Controller.Controller where

import Model.Model
import Model.Player
import View.Scaling
import Controller.Physics
import Controller.Entity
import Controller.LevelUpdate
import Graphics.Gloss.Interface.IO.Game
import Debug.Trace (traceIO)

-- import System.Exit

directKey :: [SpecialKey]
directKey = [KeyDown,KeyUp,KeyLeft,KeyRight,KeySpace,KeyShiftL]

step :: Float -> GameState -> IO GameState
step secs gstate = do
  -- print (level gstate)
  -- putStrLn "\n"
  -- print (players gstate)
  -- print (map pMovement (players gstate))
  return $ entityInteractions secs $ applyPhysics secs $ levelUpdate $ entityUpdate gstate {time = time gstate + secs}

-- gameChange :: GameState -> GameState
-- gameChange g = 

-- | Handle user input
input :: Event -> GameState -> IO GameState
-- input e g =
--   do
--     putStrLn $ show e
--     return g
input e gstate = 
  do 
    -- traceIO $ show e
    return $ (inputKey e . resizeEvent e) gstate
-- input _ = return

resizeEvent :: Event -> GameState -> GameState
resizeEvent (EventResize (x,y)) g = windowScaling (x,y) g
resizeEvent _ g = g

windowScaling :: Resolution -> GameState -> GameState
windowScaling (x,y) g = g {windowScale = sx, entityScale = sx*4, reScaled = True}
  where
    fac  = fromIntegral y / fromIntegral x
    lead 
      | fac<0.75  = y `div` 12
      | otherwise = x `div` 16
    (sx,sy) = (fromIntegral (lead*16)/1024.0,fromIntegral (lead*12)/768.0)

inputKey :: Event -> GameState -> GameState
inputKey e@(EventKey (SpecialKey key) state _ _) gstate
  | dkey = playerMove e gstate
  | otherwise =
    case key of
      KeyEsc   -> error (show (slidingWindow gstate))--exitSuccess
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

