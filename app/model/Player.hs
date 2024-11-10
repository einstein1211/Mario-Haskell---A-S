module Model.Player where

import Model.Basic
import Graphics.Gloss

data Movement   = STANDING | WALKING | CROUCHING | JUMPING -- | SKIDDING
    deriving (Show,Eq)
data Status     = SMALL | BIG       | FIRE
    deriving (Show,Eq)

instance PhysicsFunctions Player where
    getPos p = pos $ physics $ pType p
    getVel p = vel $ physics $ pType p
    getAcc p = acc $ physics $ pType p
    getDir p = dir $ physics $ pType p
    getHitbox p = htb $ physics $ pType p
    isAlive p = alive (pType p) == ALIVE  
    moveBy (x,y) p = p {pType = (pType p) {physics = pphys{pos=(px+x,py+y)}}} 
      where
        ent = pType p
        pphys = physics ent
        (px,py) = getPos p
    kill p = p {pType = (pType p) {alive = DEAD}}
    isGrounded p = gnd (physics (pType p)) == GROUNDED

-- | Data describing players in Game 
data Player = MkPlayer
    {   pType :: Entity
    ,   pMovement :: Movement
    ,   pPower :: Status
    ,   pJumpTime :: Float
    ,   pLives :: Float
    } deriving (Show,Eq)

mario :: Player
mario = MkPlayer
    {   pType = MkEntity {entity = MkPlayerType MARIO, physics = initPhysics, alive = ALIVE}
    ,   pMovement = STANDING
    ,   pPower = SMALL
    ,   pJumpTime = 0
    ,   pLives = 1
    }

initPhysics :: Physics
initPhysics = MkPhysics
    {   pos = (-300.0,0.0)
    ,   vel = (0.0,3000.0)
    ,   mxv = (500,500)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = MkHB 12 16
    ,   dir = RIGHT
    }