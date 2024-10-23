module View.Scaling where

import Model.Basic
import Model.Player
import Model.Enemy
import Model.Item

class Scale a where
  scaleTo :: a -> Hitbox

instance Scale Player where
  scaleTo player    = (\MkHB x y -> MkHB x*scaling y*scaling) htb $ physics $ pType player

instance Scale Enemy where
  scaleTo enemy     = (\MkHB x y -> MkHB x*scaling y*scaling) htb $ physics $ eType enemy

instance Scale Item where
  scaleTo item      = (\MkHB x y -> MkHB x*scaling y*scaling) htb $ physics $ iType item

instance Scale Block where
  scaleTo block     = (\MkHB x y -> MkHB x*scaling y*scaling) pfHitbox $ bType block

instance Scale Platform where
  scaleTo platform  = (\MkHB x y -> MkHB x*scaling y*scaling) pfHitbox $ platform
