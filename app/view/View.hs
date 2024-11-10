module View.View where

import Model.Model
import Model.Basic
import Model.Player
import Model.Enemy
import Model.Block
import Model.Platform
import Model.Item
import View.Images
import View.Scaling
import Controller.Physics
import Graphics.Gloss


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

imageToPicture :: Image -> Picture
imageToPicture img =
  Bitmap $ bitmapDataOfByteString
    (round width)
    (round height)
    (BitmapFormat BottomToTop PxRGBA)
    (bytestring img)
    False
  where
    MkHB width height = hitbox img

-- takes a list of frames, a number (higher number = faster animation) and time.
animateFrames :: [Image] -> Float -> Float -> Image
animateFrames frames rate time =
    let
        frameCount = length frames
        currentFrameIndex = floor (time * rate) `mod` frameCount
    in frames !! currentFrameIndex

viewPure :: GameState -> Picture
viewPure g@MkGameState {windowScale = wScale, windowRes = (width, height), mode = gameMode} =
  case gameMode of
    StartMenu -> windowToRatio wScale $ pictures [startText, startOption, quitOption]
      where
        startText   = color white $ translate (-100) 100 $ scale 0.5 0.5 (text "Super Haskell Bros")
        startOption = color green $ translate (-100) 50  $ scale 0.3 0.3 (text "Press S to start")
        quitOption  = color red   $ translate (-100) 0   $ scale 0.3 0.3 (text "Press Q to quit")
    Playing -> windowToRatio wScale $ pictures $
                  [debug] ++ gameElements ++ [pauseOverlay | isPaused g]
      where
        gameElements = viewPlayer g (players g) ++ viewEnemy g (enemies g) ++ viewPlatform g (platforms g) ++ viewBlock g (blocks g) ++ viewItem g (items g)
        -- Pause
        pauseOverlay = pausebg <> pausetext
        pausebg = color (makeColor 0 0 0 0.2) $ translate 0 0 $ rectangleSolid (fromIntegral width) (fromIntegral height)
        pausetext = color white $ translate (-100) 200 $ scale 0.5 0.5 (text "Paused")
        -- Debug
        dbtext    = color green   $ translate (-100) 200 $ scale 0.3 0.3   (text "Debug Mode")
        postext   = color magenta $ translate (-100) 170 $ scale 0.15 0.15 (text ("Pos:" ++ show (getPos player)))
        veltext   = color yellow  $ translate (-100) 140 $ scale 0.15 0.15 (text ("Vel:" ++ show (getVel player)))
        acctext   = color orange  $ translate (-100) 110 $ scale 0.15 0.15 (text ("Acc:" ++ show (getAcc player)))
        scaletext = color cyan    $ translate (-100) 80  $ scale 0.15 0.15 (text ("Escale:" ++ show es ++ " " ++ "Wscale:" ++ show ws))
        
        debug
          | debugMode g = dbtext <> postext <> veltext <> acctext <> scaletext
          | otherwise = blank

        player = head (players g)
        (MkHB w h) = getHitbox player
        (vx,vy) = getVel player
        (ax,ay) = getAcc player
        (es,ws) = (entityScale g, windowScale g)

viewPlayer :: GameState -> [Player] -> [Picture]
viewPlayer _ [] = [blank]
viewPlayer g (pl:pls) =
  case entity (pType pl) of
    MkPlayerType MARIO -> bmp : hbox : viewPlayer g pls
    _     -> [blank]
    where
      (stand, walk, jump, crouch) =
        case pPower pl of
          SMALL -> (marioStand, [mariof1, mariof2, mariof3], marioJump, marioStand)
          BIG -> (superMarioStand, [superMariof1, superMariof2, superMariof3], superMarioJump, superMarioCrouch)
          -- FIRE ->
      phys = physics (pType pl)
      img
        | not (isGrounded pl) = jump
        | pMovement pl == WALKING = animateFrames walk 10 $ time g
        | pMovement pl == CROUCHING = crouch
        | otherwise = stand
      -- framesWalking = [mariof1, mariof2, mariof3]
      bmp
        | dir phys == LEFT = uncurry translate (pos phys)$ flipPicture $ Scale s s $ imageToPicture img
        | otherwise        = uncurry translate (pos phys)$ Scale s s $ imageToPicture img
      hbox
        | debugMode g = color green $ line [(x-(w/2),y-(h/2)),(x+(w/2),y-(h/2)),(x+(w/2),y+(h/2)),(x-(w/2),y+(h/2)),(x-(w/2),y-(h/2))]
        | otherwise   = blank
      (x,y) = pos phys
      MkHB w h = htb $ physics $ pType pl
      s = entityScale g

flipPicture :: Picture -> Picture
flipPicture = Scale (-1) 1

viewEnemy :: GameState -> [Enemy] -> [Picture]
viewEnemy _ [] = [blank]
viewEnemy g (en:ens) = bmp : hbox : viewEnemy g ens
  where
      -- FIXME: DA HEK IS GOING ON HERE 
      phys = physics (eType en)
      img = 
          case entity (eType en) of
            MkEnemyType GOOMBA   -> animateFrames framesGoomba 5 $ time g
            MkEnemyType GRNKOOPA -> animateFrames framesKoopa 5 $ time g
            _                    -> undefined
      framesGoomba = [goombaWalk1, goombaWalk2]
      framesKoopa = [greenKoopaf1, greenKoopaf2]
      bmp
        | gnd phys == GROUNDED = translate x y $ Scale s s $ imageToPicture img
        | otherwise = translate x y $ Scale s s $ rotate 180 $ imageToPicture img
      hbox
        | debugMode g = color green $ line [(x-(w/2),y-(h/2)),(x+(w/2),y-(h/2)),(x+(w/2),y+(h/2)),(x-(w/2),y+(h/2)),(x-(w/2),y-(h/2))]
        | otherwise   = blank
      (x,y) = pos phys
      MkHB w h = htb $ physics $ eType en
      s = entityScale g

viewItem :: GameState -> [Item] -> [Picture]
viewItem _ [] = [blank]
viewItem g (it:its) = bmp : viewItem g its
    where
      phys = physics (iType it)
      img =
        case entity (iType it) of
          MkItemType COIN     -> animateFrames framesCoin 10 $ time g
          MkItemType MUSHROOM -> mushroom1
          _    -> undefined
      framesCoin = [coin1f1, coin1f2, coin1f3, coin1f2, coin1f1]
      bmp =
        case entity (iType it) of
        MkItemType COIN -> translate x y $ Scale s s $ imageToPicture img
        _               -> uncurry translate (pos (physics (iType it))) $ Scale s s $ imageToPicture img
      (x,y) = gridPos (iPos it) ws
      (MkHB width height) = hitbox img
      s = entityScale g
      ws = windowScale g

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
    bmp | img /= noimg = translate x y $ Scale es es $ imageToPicture img
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
    bmp | img /= noimg = translate x y $ Scale es es $ imageToPicture img
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