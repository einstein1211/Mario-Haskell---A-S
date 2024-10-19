module Model.Players where

import Model.Basic

data PlyrType   = MARIO | LUIGI
    deriving (Show,Eq)
data Movement   = NORMAL| RUNNING   | CROUCHED
    deriving (Show,Eq)
data Status     = SMALL | BIG       | FIRE
    deriving (Show,Eq)

-- | Data describing players in Game 
data Player = Player
    {   plyType :: PlyrType
    ,   plyPhysics :: Physics
    ,   plyDirection :: Direction
    ,   plyAlive :: IsAlive
    ,   plyMovement :: Movement
    ,   plyPower :: Status
    ,   plyJumpTime :: Float
    } deriving (Show,Eq)

mario :: Player
mario = Player
    {   plyType = MARIO
    ,   plyPhysics = initPhysics
    ,   plyDirection = RIGHT
    ,   plyAlive = ALIVE
    ,   plyMovement = NORMAL
    ,   plyPower = SMALL
    ,   plyJumpTime = 0
    }

initPhysics :: Physics
initPhysics = Physics
    {   pos = (0.0,0.0)
    ,   vel = (0.0,3000.0)
    ,   mxv = (500,500)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = HB 12 16
    }