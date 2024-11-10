module View.Scaling where

import Model.Basic
import Model.Player
import Model.Enemy
import Model.Item
import Model.Block
import Model.Platform

import Graphics.Gloss

windowToRatio :: Scaling -> Picture -> Picture
windowToRatio s = scale s s

imgscale :: Float
imgscale = 4

class Scale a where
  scaleTo :: Float -> a -> a

instance Scale Player where
  scaleTo s player = player{pType = typ {physics = phys {htb = hitboxScale hitbox s}}}
    where
      typ = pType player
      phys  = physics typ
      hitbox= htb phys

instance Scale Enemy where
  scaleTo s enemy = enemy{eType = typ {physics = phys {htb = hitboxScale hitbox s}}}
    where
      typ = eType enemy
      phys  = physics typ
      hitbox= htb phys

instance Scale Item where
  scaleTo s item = item {iType = typ {physics = phys {htb = hitboxScale hitbox s}}}
    where
      typ = iType item
      phys  = physics typ
      hitbox= htb phys

instance Scale Block where
  scaleTo s block = block {bPlatform = plat {pfHitbox = hitboxScale hitbox s}}
    where
      plat = bPlatform block
      hitbox= pfHitbox plat

instance Scale Platform where
  scaleTo s platform = platform {pfHitbox = hitboxScale hitbox s}
    where
      hitbox= pfHitbox platform

hitboxScale :: Hitbox -> Float -> Hitbox
hitboxScale (MkHB w h) s = MkHB (w*s) (h*s)