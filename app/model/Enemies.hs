module Model.Enemies where

import Model.Basic

data EnmyType   = GOOMBA| GRNKOOPA  | REDKOOPA | SPINY | PIRANHA
    deriving (Show,Eq)
data EnmyAI     = EASY  | MEDIUM    | HARD
    deriving (Show,Eq)

-- | Data describing enemies in Game 
data Enemy = Enemy
    {   eType :: EnmyType
    ,   ePhysics :: Physics
    ,   eDirection :: Direction
    ,   eAlive :: IsAlive
    ,   eAI :: EnmyAI
    } deriving (Show,Eq)

goomba :: Enemy
goomba = Enemy
    {   eType = GOOMBA
    ,   ePhysics = initPhysics2
    ,   eDirection = RIGHT
    ,   eAlive = ALIVE
    ,   eAI = EASY
    }

goomba2 :: Enemy
goomba2 = Enemy
    {   eType = GOOMBA
    ,   ePhysics = initPhysics3
    ,   eDirection = RIGHT
    ,   eAlive = ALIVE
    ,   eAI = EASY
    }

initPhysics2 :: Physics
initPhysics2 = Physics
    {   pos = (0.0,0.0)
    ,   vel = (200.0,300.0)
    ,   mxv = (3000,3000)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = HB 14 16    
    }

initPhysics3 :: Physics
initPhysics3 = Physics
    {   pos = (0.0,0.0)
    ,   vel = (-200.0,300.0)
    ,   mxv = (3000,3000)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = HB 14 16    
    }