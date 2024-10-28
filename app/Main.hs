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

-- animated sprites unproven method 1
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

-- animated sprites unproven method 2
-- -- Display the animation by looping through frames
-- animateFrames :: [Picture] -> Float -> Picture
-- animateFrames frames time = 
--     let
--         frameCount = length frames
--         currentFrameIndex = floor (time * 10) `mod` frameCount
--     in frames !! currentFrameIndex

-- main :: IO ()
-- main = do
--     frames <- loadFrames ["frame1.bmp", "frame2.bmp", "frame3.bmp", "frame4.bmp"] -- Load your frames here
--     animate $ animateFrames frames

animateFrames :: [Picture] -> Float -> Picture
animateFrames frames time = 
    let
        frameCount = length frames
        currentFrameIndex = floor (time * 10) `mod` frameCount
    in frames !! currentFrameIndex

frames = loadFrames [, , ] -- load frames
animate (animateFrames frames)