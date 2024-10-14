module Controller.Controller where

import Model
import Controller.Physics
-- import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameState -> IO GameState
step secs gstate =
  return $ physics (secs) gstate

-- | Handle user input
input :: Event -> GameState -> IO GameState
-- input e gstate = return (inputKey e gstate)
input _ = return

-- inputKey :: Event -> GameState -> GameState
-- inputKey (EventKey (Char c) _ _ _) gstate
--   = -- If the user presses a character key, show that one
--     -- gstate { infoToShow = ShowAChar c }
--     gstate
-- inputKey _ gstate = gstate -- Otherwise keep the same