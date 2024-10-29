module View.View where

import Model.Model
import Model.Basic
import Model.Player
import Model.Enemy
import Model.Block
import Model.Platform
import Model.Level
import View.Images
import View.Scaling
import Controller.Physics
import Graphics.Gloss

import qualified Data.Map as Map

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
viewPure g@MkGameState {windowScale = wScale} =
  -- windowToRatio wScale $ pictures $ debug : viewLevel g (level g) ++ viewPlayer g (players g)
  -- windowToRatio wScale $ pictures $ debug : viewColumn g (column g)
  windowToRatio wScale $ pictures $ debug : viewPlayer g (players g) ++ viewEnemy g (enemies g) ++ viewPlatform g (platforms g) ++ viewBlock g blocks
  where
    blocks    = map (scaleTo (entityScale g)) $ Map.foldr (\c ac -> getEntries c++ac) [] (slidingWindow g)
    dbtext    = color green   $ translate (-100) 200 $ scale 0.3 0.3   (text "Debug Mode")
    postext   = color magenta $ translate (-100) 170 $ scale 0.15 0.15 (text ("Pos:" ++ show (getPos player)))
    veltext   = color yellow  $ translate (-100) 140 $ scale 0.15 0.15 (text ("Vel:" ++ show (getVel player)))
    acctext   = color orange  $ translate (-100) 110 $ scale 0.15 0.15 (text ("Acc:" ++ show (getAcc player)))
    scaletext = color cyan    $ translate (-100) 80 $ scale 0.15 0.15 (text ("Escale:" ++ show es ++ " " ++ "Wscale:" ++ show ws))
    player = head (players g)
    (MkHB w h) = getHitbox player
    (vx,vy) = getVel player
    (ax,ay) = getAcc player
    (es,ws) = (entityScale g,windowScale g)
    debug
      | debugMode g = dbtext <> postext <> veltext <> acctext <> scaletext
      | otherwise = blank

viewPlayer :: GameState -> [Player] -> [Picture]
viewPlayer _ [] = [blank]
viewPlayer g (pl:pls) =
  case entity (pType pl) of
    MkPlayerType MARIO -> bmp : hbox : viewPlayer g pls
    _     -> [blank]
    where
      phys = physics (pType pl)
      img = if gnd phys == GROUNDED then marioStand else marioJump
      bmp = uncurry translate (pos phys)$ Scale s s $ Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) False
      MkHB width height = hitbox img
      hbox
        | debugMode g = color green $ line [(x-(w/2),y-(h/2)),(x+(w/2),y-(h/2)),(x+(w/2),y+(h/2)),(x-(w/2),y+(h/2)),(x-(w/2),y-(h/2))]
        | otherwise   = blank
      (x,y) = pos phys
      MkHB w h = htb $ physics $ pType pl
      s = entityScale g

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
        | gnd phys == GROUNDED = translate x y $ Scale s s bmp'
        | otherwise = translate x y $ Scale s s $ rotate 180 bmp'
      bmp' =  Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) False
      MkHB width height = hitbox img
      hbox
        | debugMode g = color green $ line [(x-(w/2),y-(h/2)),(x+(w/2),y-(h/2)),(x+(w/2),y+(h/2)),(x-(w/2),y+(h/2)),(x-(w/2),y-(h/2))]
        | otherwise   = blank
      (x,y) = pos phys
      MkHB w h = htb $ physics $ eType en
      s = entityScale g

viewLevel :: GameState -> Level -> [Picture]
viewLevel g = Map.foldl f [Blank]
  where
    f ac c = viewColumn g c <> ac

viewColumn :: GameState -> Column -> [Picture]
viewColumn _ (MkColumn []) = [blank]
-- viewColumn g (MkColumn ((MkTile s NoChunk grid):ts)) = ent : viewColumn g (MkColumn ts)
viewColumn g (MkColumn ((MkTile _ NoChunk _):ts)) = blank : viewColumn g (MkColumn ts)
viewColumn g (MkColumn ((MkTile _ c _):ts)) = obstacle : viewColumn g (MkColumn ts)
  where
    obstacle =
      case c of
        MkBlkChunk b    -> viewBlock2 g b
        MkPltChunk p    -> viewPlatform2 g p
    -- ent = 
    --   case s of
    --     MkPlayer    -> undefined
    --     MkEnemy     -> undefined
    --     MkItem      -> undefined
    


viewPlatform :: GameState -> [Platform] -> [Picture]
viewPlatform _ [] = [blank]
viewPlatform g (plt:plts) = bmp : hbox : viewPlatform g plts
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
    bmp | img /= noimg = translate x y $ Scale es es $ Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) False
        | otherwise = blank
    hbox
        | debugMode g = color green $ line [(x-(w/2),y-(h/2)),(x+(w/2),y-(h/2)),(x+(w/2),y+(h/2)),(x-(w/2),y+(h/2)),(x-(w/2),y-(h/2))]
        | otherwise   = blank
    (x,y) = gridPos (pfPos plt) ws
    MkHB w h = pfHitbox plt
    es = entityScale g
    ws = windowScale g

viewPlatform2 :: GameState -> Platform -> Picture
viewPlatform2 g plt = bmp <> hbox
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
    bmp | img /= noimg = translate x y $ Scale es es $ Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) False
        | otherwise = blank
    hbox
        | debugMode g = color green $ line [(x-(w/2),y-(h/2)),(x+(w/2),y-(h/2)),(x+(w/2),y+(h/2)),(x-(w/2),y+(h/2)),(x-(w/2),y-(h/2))]
        | otherwise   = blank
    (x,y) = gridPos (pfPos plt) ws
    MkHB w h = pfHitbox plt
    es = entityScale g
    ws = windowScale g

viewBlock :: GameState -> [Block] -> [Picture]
viewBlock _ [] = [blank]
viewBlock g (blck:blcks) = bmp : hbox : viewBlock g blcks
  where
    img =
      case bType blck of
        BRICK       -> brick1
        QBLOCK      -> question1f1
        EMPTYBLOCK  -> emptyblock1
        HIDDENBLOCK -> noimg
    MkHB width height = hitbox img
    bmp | img /= noimg = translate x y $ Scale es es $ Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) False
        | otherwise = blank
    hbox
        | debugMode g = color green $ line [(x-(w/2),y-(h/2)),(x+(w/2),y-(h/2)),(x+(w/2),y+(h/2)),(x-(w/2),y+(h/2)),(x-(w/2),y-(h/2))]
        | otherwise   = blank
    (x,y) = gridPos (pfPos (bPlatform blck)) ws
    MkHB w h = pfHitbox (bPlatform blck)
    es = entityScale g
    ws = windowScale g

viewBlock2 :: GameState -> Block -> Picture
viewBlock2 g blck = bmp <> hbox
  where
    img =
      case bType blck of
        BRICK       -> brick1
        QBLOCK      -> question1f1
        EMPTYBLOCK  -> emptyblock1
        HIDDENBLOCK -> noimg
    MkHB width height = hitbox img
    bmp | img /= noimg = translate x y $ Scale es es $ Bitmap $ bitmapDataOfByteString (round width) (round height) (BitmapFormat BottomToTop PxRGBA) (bytestring img) False
        | otherwise = blank
    hbox
        | debugMode g = color green $ line [(x-(w/2),y-(h/2)),(x+(w/2),y-(h/2)),(x+(w/2),y+(h/2)),(x-(w/2),y+(h/2)),(x-(w/2),y-(h/2))]
        | otherwise   = blank
    (x,y) = gridPos (pfPos (bPlatform blck)) ws
    MkHB w h = pfHitbox (bPlatform blck)
    es = entityScale g
    ws = windowScale g
-- viewPure :: GameState -> Picture
-- viewPure gstate = case infoToShow gstate of
--   ShowNothing   -> blank
--   ShowANumber n -> color green (text (show n))
--   ShowAChar   c -> color green (text [c])
--   ShowAShape  o -> viewObject o