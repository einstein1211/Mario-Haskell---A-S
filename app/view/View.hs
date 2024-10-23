module View.View where

import Model.Model
import Model.Basic
import Model.Player
import Model.Enemy
import Model.Block
import Model.Platform
import View.Images
import Graphics.Gloss
import Data.Bifunctor

-- viewObject :: Color -> Point -> Path -> Picture
-- viewObject c p pt =
--   color c $ polygon $ locup p pt
--     where
--       locup :: Point -> Path -> Path
--       locup _ [] = []
--       locup l (s:ss) = bimap (fst l +) (snd l +) s: locup l ss

view :: GameState -> IO Picture
view g = do
  -- putStrLn $ show $ pressedKeys g
  (return . viewPure) g

viewPure :: GameState -> Picture
viewPure g = pictures $ debug : viewPlayer g (players g) ++ viewEnemy g (enemies g) ++ viewPlatform g (platforms g) ++ viewBlock g (blocks g)
  where
    dbtext  = color green   $ translate (-100) 200 $ scale 0.3 0.3   (text "Debug Mode")
    postext = color magenta $ translate (-100) 170 $ scale 0.15 0.15 (text ("Pos:" ++ show (getPos player)))
    veltext = color yellow  $ translate (-100) 140 $ scale 0.15 0.15 (text ("Vel:" ++ show (getVel player)))
    acctext = color orange  $ translate (-100) 110 $ scale 0.15 0.15 (text ("Acc:" ++ show (getAcc player)))
    player = head (players g)
    (MkHB w h) = getHitbox player
    (vx,vy) = getVel player
    (ax,ay) = getAcc player
    debug
      | debugMode g = dbtext <> postext <> veltext <> acctext
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
      MkHB w h = htb $ physics $ pType pl

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
      MkHB w h = htb $ physics $ eType en

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