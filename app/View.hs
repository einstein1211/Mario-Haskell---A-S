module View where

import Model
import Images
import Graphics.Gloss

viewObject :: Color -> Point -> Path -> Picture
viewObject c p pt =
  color c $ polygon $ locup p pt
    where
      locup :: Point -> Path -> Path
      locup _ [] = []
      locup l (s:ss) = (fst l + fst s,snd l + snd s): locup l ss

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures $ viewPlayer (players gstate) ++ viewEnemy (enemies gstate)

viewPlayer :: [Player] -> [Picture]
viewPlayer [] = [blank]
viewPlayer (pl:pls) =
  case plyType pl of
    -- Mario -> viewObject green (pos (plyPhysics pl)) marioPath : viewPlayer pls
    Mario -> bmp : viewPlayer pls
    _     -> [blank]
    where 
      bmp = uncurry translate (pos (plyPhysics pl)) $ Scale 20 20 $ Bitmap $ bitmapDataOfByteString 12 16 (BitmapFormat BottomToTop PxRGBA) mariobmp True

viewEnemy :: [Enemy] -> [Picture]
viewEnemy [] = [blank]
viewEnemy (en:ens) =
  case eType en of
    -- GOOMBA -> viewObject orange (pos (ePhysics en)) marioPath : viewEnemy ens
    GOOMBA -> bmp : viewEnemy ens
    _     -> [blank]
    where
      bmp = uncurry translate (pos (ePhysics en)) $ Scale 20 20 $ Bitmap $ bitmapDataOfByteString 16 16 (BitmapFormat BottomToTop PxRGBA) goombabmp True
-- viewPure :: GameState -> Picture
-- viewPure gstate = case infoToShow gstate of
--   ShowNothing   -> blank
--   ShowANumber n -> color green (text (show n))
--   ShowAChar   c -> color green (text [c])
--   ShowAShape  o -> viewObject o