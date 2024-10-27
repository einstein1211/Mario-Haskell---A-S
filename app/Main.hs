module Main where
import Model.Model
import Model.Basic
import Controller.Controller
import View.View
import View.Scaling
import Graphics.Gloss.Interface.IO.Game

window :: Display
window = InWindow "Mario" res (0,0)

main :: IO ()
main = playIO window
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


-- Load your frames as images
--     do
--     frame1 <- loadBMP "frame1.bmp"
--     frame2 <- loadBMP "frame2.bmp"
--     let frames = [frame1, frame2]
--     animate (InWindow "Sprite Animation" (800, 600) (100, 100)) white (frameRenderer frames)

-- frameRenderer :: [Picture] -> Float -> Picture
-- frameRenderer frames time =
--     let frameCount = length frames
--         currentFrame = frames !! (floor (time * 10) `mod` frameCount)
--     in currentFrame