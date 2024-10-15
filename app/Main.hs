module Main where
import Model
import Controller.Controller
import View
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Mario" res (0, 0))
              blue            -- Background color
              fps               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function


-- loadBitmaps :: IO [(String,Picture)]
-- loadBitmaps = mapM (\s -> (\b -> (s,b)) <$> loadBMP s) images

-- images :: [String]
-- images = []