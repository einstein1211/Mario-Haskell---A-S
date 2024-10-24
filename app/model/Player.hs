{-# LANGUAGE InstanceSigs #-}
module Model.Player where

import Model.Basic
import Graphics.Gloss

data Movement   = STANDING | RUNNING | CROUCHED | JUMPING
    deriving (Show,Eq)
data Status     = SMALL | BIG       | FIRE
    deriving (Show,Eq)

instance GetPhysics Player where
    getPos :: Player -> Point
    getPos p = pos $ physics $ pType p
    getVel :: Player -> Velocity
    getVel p = vel $ physics $ pType p
    getAcc :: Player -> Acceleration
    getAcc p = acc $ physics $ pType p
    getHitbox :: Player -> Hitbox
    getHitbox p = htb $ physics $ pType p
    isAlive :: Player -> Bool
    isAlive p = alive (pType p) == ALIVE  

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
    ,   pLives = 3
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