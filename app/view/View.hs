module View.View where

import Model.Model
import Model.Basic
import Model.Platforms
import Model.Enemies
import Model.Players
import Model.Blocks
import Model.Items
import View.Images
import Graphics.Gloss
import Data.Bifunctor

viewObject :: Color -> Point -> Path -> Picture
viewObject c p pt =
  color c $ polygon $ locup p pt
    where
      locup :: Point -> Path -> Path
      locup _ [] = []
      locup l (s:ss) = bimap (fst l +) (snd l +) s: locup l ss

view :: GameState -> IO Picture
view g = do
  -- putStrLn $ show $ pressedKeys g
  (return . viewPure) g

viewPure :: GameState -> Picture
viewPure gstate = pictures $ viewPlayer (players gstate) ++ viewEnemy (enemies gstate) ++ viewPlatform (platforms gstate) ++ viewBlock (blocks gstate)

--TODO: Implement scale function

viewPlayer :: [Player] -> [Picture]
viewPlayer [] = [blank]
viewPlayer (pl:pls) =
  case plyType pl of
    MARIO -> bmp : viewPlayer pls
    _     -> [blank]
    where
      img = if gnd (plyPhysics pl) == GROUNDED then marioStand else marioJump
      bmp = uncurry translate (pos (plyPhysics pl))$ Scale scaling scaling $ Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) False
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
        | gnd (ePhysics en) == GROUNDED = uncurry translate (pos (ePhysics en))$ Scale scaling scaling bmp'
        | otherwise = uncurry translate (pos (ePhysics en))$ Scale scaling scaling $ rotate 180 bmp'
      bmp' =  Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) False
      HB width height = hitbox img

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
    HB width height = hitbox img
    bmp = uncurry translate (gridPos (pltPos plt)) $ Scale scaling scaling $ Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) False

viewBlock :: [Block] -> [Picture]
viewBlock [] = [blank]
viewBlock (blck:blcks) = bmp : viewBlock blcks
  where
    img =
      case bType blck of
        BRICK       -> brick1
        QBLOCK      -> question1f1
        EMPTYBLOCK  -> emptyblock1
        INVISBLOCK  -> undefined
    HB width height = hitbox img
    bmp = uncurry translate (gridPos (bPos blck)) $ Scale scaling scaling $ Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) False

-- viewPure :: GameState -> Picture
-- viewPure gstate = case infoToShow gstate of
--   ShowNothing   -> blank
--   ShowANumber n -> color green (text (show n))
--   ShowAChar   c -> color green (text [c])
--   ShowAShape  o -> viewObject o