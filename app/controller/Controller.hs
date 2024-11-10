module Controller.Controller where

import Model.Model
import Model.Player
import View.Scaling
import Controller.Physics
import Controller.Entity
import Graphics.Gloss.Interface.IO.Game
import System.Exit (exitSuccess)
import Debug.Trace (trace)

-- trace :: String -> a -> a

directKey :: [SpecialKey]
directKey = [KeyDown,KeyUp,KeyLeft,KeyRight,KeySpace,KeyShiftL]

-- Non debug step
-- step :: Float -> GameState -> IO GameState
-- step secs gstate
--   | mode gstate == StartMenu = return gstate
--   | isPaused gstate = return gstate
--   | otherwise = return $ entityInteractions secs $ applyPhysics secs $ entityUpdate gstate { time = time gstate + secs }

--Debug step
step :: Float -> GameState -> IO GameState --startmenu
step secs gstate
  | mode gstate == StartMenu = return gstate
  | isPaused gstate = return gstate
  | otherwise = return $ trace "Before entityUpdate" $
                trace (show (time gstate)) $
                entityInteractions secs $ 
                applyPhysics secs $ 
                trace "After entityUpdate" $ 
                entityUpdate gstate { time = time gstate + secs }

-- Handle s en q tijdens het start menu
input :: Event -> GameState -> IO GameState
input e gstate = case e of
  EventKey (Char 's') Down _ _ | mode gstate == StartMenu -> return gstate { mode = Playing }
  EventKey (Char 'q') Down _ _ | mode gstate == StartMenu -> exitSuccess
  EventKey (Char 'p') Down _ _ | mode gstate == Playing   -> return gstate { isPaused = not (isPaused gstate) }
  _ | mode gstate == Playing -> return $ (inputKey e . resizeEvent e) gstate
  _ -> return gstate

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


