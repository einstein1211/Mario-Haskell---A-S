module Model.Player where

import Model.Basic

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
    ,   pInvTime :: Float
    } deriving (Show,Eq)

-- Data describing player 1
mario :: Player
mario = MkPlayer
    {   pType = MkEntity {entity = MkPlayerType MARIO, physics = marioPhysics , alive = ALIVE}
    ,   pMovement = STANDING
    ,   pPower = SMALL
    ,   pJumpTime = 0
    ,   pLives = 1
    ,   pInvTime = 0
    }

-- Initial physics for player 1
marioPhysics :: Physics
marioPhysics = MkPhysics
    {   pos = makeGridPos (4,9) startScaling
    ,   vel = (0.0,3000.0)
    ,   mxv = (500,500)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = MkHB 12 16
    ,   dir = RIGHT
    }