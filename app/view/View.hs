module View.View where

import Model.Model
import Model.Basic
import Model.Player
import Model.Enemy
import Model.Platform
import View.Images
import Graphics.Gloss
import Data.Bifunctor
import Model.Basic (EntityType(MkPlayerType, MkBlockType))

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
viewPure gstate = pictures $ viewPlayer (players gstate) ++ viewEnemy (enemies gstate) ++ viewPlatform (platforms gstate)

--TODO: Implement scale function

viewPlayer :: [Player] -> [Picture]
viewPlayer [] = [blank]
viewPlayer (pl:pls) =
  case entity (pType pl) of
    MkPlayerType MARIO -> bmp : viewPlayer pls
    _     -> [blank]
    where
      phys = physics (pType pl)
      img = if gnd phys == GROUNDED then marioStand else marioJump
      bmp = uncurry translate (pos phys)$ Scale scaling scaling $ Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) False
      (MkHB width height) = hitbox img

viewEnemy :: [Enemy] -> [Picture]
viewEnemy [] = [blank]
viewEnemy (en:ens) =
  case entity (eType en) of
    -- GOOMBA -> viewObject orange (pos (ePhysics en)) marioPath : viewEnemy ens
    MkEnemyType GOOMBA -> bmp : viewEnemy ens
    _     -> [blank]
    where
      -- FIXME: DA HEK IS GOING ON HERE 
      phys = physics (eType en)
      img = if mod (round (fst (pos phys))) 100 > 50 then goombaWalk1 else goombaWalk2
      bmp
        | gnd phys == GROUNDED = uncurry translate (pos phys)$ Scale scaling scaling $ bmp'
        | otherwise = uncurry translate (pos phys)$ Scale scaling scaling $ rotate 180 bmp'
      bmp' =  Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) False
      (MkHB width height) = hitbox img

viewPlatform :: [Platform] -> [Picture]
viewPlatform [] = [blank]
viewPlatform (plt:plts) = bmp : viewPlatform plts
  where
    img =
      case pfType plt of
        PIPEL   -> pipe_l1
        PIPER   -> pipe_r1
        PIPETL  -> pipe_tl1
        PIPETR  -> pipe_tr1
        DIRT    -> dirt1
        STAIR   -> stair1
    (MkHB width height) = hitbox img
    bmp = uncurry translate (gridPos (pfPos plt)) $ Scale scaling scaling $ Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) False

-- viewPure :: GameState -> Picture
-- viewPure gstate = case infoToShow gstate of
--   ShowNothing   -> blank
--   ShowANumber n -> color green (text (show n))
--   ShowAChar   c -> color green (text [c])
--   ShowAShape  o -> viewObject o