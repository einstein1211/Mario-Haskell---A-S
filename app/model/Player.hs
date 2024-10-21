module Model.Player where

import Model.Basic
import Model.Basic (Physics(MkPhysics))

data Movement   = NORMAL| RUNNING   | CROUCHED
    deriving (Show,Eq)
data Status     = SMALL | BIG       | FIRE
    deriving (Show,Eq)

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
    ,   pMovement = NORMAL
    ,   pPower = SMALL
    ,   pJumpTime = 0
    }

initPhysics :: Physics
initPhysics = MkPhysics
    {   pos = (0.0,0.0)
    ,   vel = (0.0,3000.0)
    ,   mxv = (500,500)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = MkHB 12 16
    ,   dir = RIGHT
    }