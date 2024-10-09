module Main where

import Model
import Controller
import View
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Counter" (1280, 720) (0, 0))
              black            -- Background color
              fps               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
