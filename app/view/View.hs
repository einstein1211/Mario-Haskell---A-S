module View.View where

import Model.Model
import Model.Basic
import Model.Platforms
import View.Images
import Graphics.Gloss

viewObject :: Color -> Point -> Path -> Picture
viewObject c p pt =
  color c $ polygon $ locup p pt
    where
      locup :: Point -> Path -> Path
      locup _ [] = []
      locup l (s:ss) = (fst l + fst s,snd l + snd s): locup l ss

view :: GameState -> IO Picture
view g = do
  -- putStrLn $ show $ pressedKeys g
  (return . viewPure) g

viewPure :: GameState -> Picture
viewPure gstate = pictures $ viewPlayer (players gstate) ++ viewEnemy (enemies gstate) ++ viewPlatform (platforms gstate)

--TODO: Implement scale function

viewPlayer :: [Player] -> [Picture]
viewPlayer [] = [blank]
viewPlayer (pl:pls) =
  case plyType pl of
    MARIO -> bmp : viewPlayer pls
    _     -> [blank]
    where
      img = if gnd (plyPhysics pl) == GROUNDED then marioStand else marioJump
      bmp = uncurry translate (pos (plyPhysics pl))$ Scale scaling scaling $ Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) True
      (HB width height) = hitbox img

viewEnemy :: [Enemy] -> [Picture]
viewEnemy [] = [blank]
viewEnemy (en:ens) =
  case eType en of
    -- GOOMBA -> viewObject orange (pos (ePhysics en)) marioPath : viewEnemy ens
    GOOMBA -> bmp : viewEnemy ens
    _     -> [blank]
    where
      -- FIXME: DA HEK IS GOING ON HERE 
      img = if mod (round (fst (pos (ePhysics en)))) 100 > 50 then goombaWalk1 else goombaWalk2
      bmp 
        | gnd (ePhysics en) == GROUNDED = uncurry translate (pos (ePhysics en))$ Scale scaling scaling $ bmp'
        | otherwise = uncurry translate (pos (ePhysics en))$ Scale scaling scaling $ rotate 180 bmp'
      bmp' =  Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) True
      (HB width height) = hitbox img

viewPlatform :: [Platform] -> [Picture]
viewPlatform [] = [blank]
viewPlatform (plt:plts) = bmp : viewPlatform plts
  where
    img =
      case pltType plt of
        PIPEL   -> pipe_l1
        PIPER   -> pipe_r1
        PIPETL  -> pipe_tl1
        PIPETR  -> pipe_tr1
        DIRT    -> dirt1
        STAIR   -> stair1
    (HB width height) = hitbox img
    bmp = uncurry translate (gridPos (pltPos plt)) $ Scale scaling scaling $ Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) True

-- viewPure :: GameState -> Picture
-- viewPure gstate = case infoToShow gstate of
--   ShowNothing   -> blank
--   ShowANumber n -> color green (text (show n))
--   ShowAChar   c -> color green (text [c])
--   ShowAShape  o -> viewObject o