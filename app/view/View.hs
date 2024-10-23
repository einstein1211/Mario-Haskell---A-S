module View.View where

import Model.Model
import Model.Basic
import Model.Player
import Model.Enemy
import Model.Block
import Model.Platform
import Model.Item
import View.Images
import Graphics.Gloss
    ( blank,
      color,
      pictures,
      polygon,
      rotate,
      translate,
      bitmapDataOfByteString,
      Color,
      Path,
      Picture(Bitmap, Scale),
      Point,
      BitmapFormat(BitmapFormat),
      PixelFormat(PxRGBA),
      RowOrder(BottomToTop) )
import Data.Bifunctor ( Bifunctor(bimap) )
import Model.Basic (EntityType(MkPlayerType, MkBlockType))
import Graphics.Gloss

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
viewPure g = pictures $ debug : viewPlayer g (players g) ++ viewEnemy g (enemies g) ++ viewPlatform g (platforms g) ++ viewBlock g (blocks g) ++ viewItem g (items g)
  where
    debug
      | debugMode g = color green $ scale 0.3 0.3 $ translate (-300) 300 (text "Debug Mode")
      | otherwise = blank

--TODO: Implement scale function

viewPlayer :: GameState -> [Player] -> [Picture]
viewPlayer _ [] = [blank]
viewPlayer g (pl:pls) =
  case entity (pType pl) of
    MkPlayerType MARIO -> bmp : hbox : viewPlayer g pls
    _     -> [blank]
    where
      phys = physics (pType pl)
      img = if gnd phys == GROUNDED then marioStand else marioJump
      bmp = uncurry translate (pos phys)$ Scale scaling scaling $ Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) False
      MkHB width height = hitbox img
      hbox 
        | debugMode g = color green $ line [(x-(w/2),y-(h/2)),(x+(w/2),y-(h/2)),(x+(w/2),y+(h/2)),(x-(w/2),y+(h/2)),(x-(w/2),y-(h/2))]
        | otherwise   = blank
      (x,y) = pos phys
      (w,h) = (width*scaling,height*scaling)

viewEnemy :: GameState -> [Enemy] -> [Picture]
viewEnemy _ [] = [blank]
viewEnemy g (en:ens) =
  case entity (eType en) of
    -- GOOMBA -> viewObject orange (pos (ePhysics en)) marioPath : viewEnemy ens
    MkEnemyType GOOMBA -> bmp : hbox : viewEnemy g ens
    _     -> [blank]
    where
      -- FIXME: DA HEK IS GOING ON HERE 
      phys = physics (eType en)
      img = if mod (round (fst (pos phys))) 100 > 50 then goombaWalk1 else goombaWalk2
      bmp
        | gnd phys == GROUNDED = uncurry translate (pos phys)$ Scale scaling scaling $ bmp'
        | otherwise = uncurry translate (pos phys)$ Scale scaling scaling $ rotate 180 bmp'
      bmp' =  Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) False
      MkHB width height = hitbox img
      hbox
        | debugMode g = color green $ line [(x-(w/2),y-(h/2)),(x+(w/2),y-(h/2)),(x+(w/2),y+(h/2)),(x-(w/2),y+(h/2)),(x-(w/2),y-(h/2))]
        | otherwise   = blank
      (x,y) = pos phys
      (w,h) = (width*scaling,height*scaling)

viewItem :: GameState -> [Item] -> [Picture]
viewItem _ [] = [blank]
viewItem g (it:its) =
  case entity (iType it) of
    MkItemType COIN -> bmp : viewItem g its   
    _    -> [blank]         
  where
    phys = physics (iType it) 
    img = coin1f1 --wont recognize coinSprite within scope even if import is edited >:@
    bmp = uncurry translate (gridPos (iPos it)) $ Scale scaling scaling $ Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) False
    (MkHB width height) = hitbox img

    -- Send help.

viewPlatform :: GameState -> [Platform] -> [Picture]
viewPlatform _ [] = [blank]
viewPlatform g (plt:plts) = bmp : viewPlatform g plts
  where
    img =
      case pfType plt of
        PIPEL   -> pipe_l1
        PIPER   -> pipe_r1
        PIPETL  -> pipe_tl1
        PIPETR  -> pipe_tr1
        DIRT    -> dirt1
        STAIR   -> stair1
        BLOCK   -> noimg
    MkHB width height = hitbox img
    bmp | img /= noimg = uncurry translate (gridPos (pfPos plt)) $ Scale scaling scaling $ Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) False
        | otherwise = blank

viewBlock :: GameState -> [Block] -> [Picture]
viewBlock _ [] = [blank]
viewBlock g (blck:blcks) = bmp : viewBlock g blcks
  where
    img =
      case bType blck of
        BRICK       -> brick1
        QBLOCK      -> question1f1
        EMPTYBLOCK  -> emptyblock1
        HIDDENBLOCK -> noimg
    MkHB width height = hitbox img
    bmp | img /= noimg = uncurry translate (gridPos (pfPos (bPlatform blck))) $ Scale scaling scaling $ Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) False
        | otherwise = blank
-- viewPure :: GameState -> Picture
-- viewPure gstate = case infoToShow gstate of
--   ShowNothing   -> blank
--   ShowANumber n -> color green (text (show n))
--   ShowAChar   c -> color green (text [c])
--   ShowAShape  o -> viewObject o