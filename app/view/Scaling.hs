module View.Scaling where

import Model.Basic
import Model.Player
import Model.Enemy
import Model.Item
import Model.Block
import Model.Platform

class Scale a where
  scaleTo :: a -> a

instance Scale Player where
  scaleTo player = player{pType = typ {physics = phys {htb = (hitboxScale hitbox)}}}
    where
      typ = pType player
      phys  = physics typ
      hitbox= htb phys

instance Scale Enemy where
  scaleTo enemy = enemy{eType = typ {physics = phys {htb = (hitboxScale hitbox)}}}
    where
      typ = eType enemy
      phys  = physics typ
      hitbox= htb phys

instance Scale Item where
  scaleTo item = item{iType = typ {physics = phys {htb = (hitboxScale hitbox)}}}
    where
      typ = iType item
      phys  = physics typ
      hitbox= htb phys

instance Scale Block where
  scaleTo block = block {bPlatform = plat {pfHitbox = (hitboxScale hitbox)}}
    where
      plat = bPlatform block
      hitbox= pfHitbox plat

instance Scale Platform where
  scaleTo platform = platform {pfHitbox = (hitboxScale hitbox)}
    where
      hitbox= pfHitbox platform 

hitboxScale :: Hitbox -> Hitbox
hitboxScale (MkHB w h) = MkHB (w*scaling) (h*scaling)