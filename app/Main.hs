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

-- lives int 
-- mario ppower == small
-- when interacting with a mushroom -> ppower = big
-- when ppower == big -> mario changes to super mario (view.hs)
-- when interacting with an enemy -> ppower = small
-- when ppower == small -> super mario changes to mario (view.hs)

