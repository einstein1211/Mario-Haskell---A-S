module Model.Enemy where

import Model.Basic

data EnemyAI = EASY | MEDIUM | HARD
    deriving (Show,Eq)

instance IsAlive Enemy where
    isAlive e = alive (eType e) == ALIVE

-- | Data describing enemies in Game 
data Enemy = MkEnemy
    {   eType :: Entity
    ,   eAI :: EnemyAI
    } deriving (Show,Eq)

goomba :: Enemy
goomba = MkEnemy
    {   eType = MkEntity {entity = MkEnemyType GOOMBA, physics = initPhysics2, alive = ALIVE}
    ,   eAI = EASY
    }

goomba2 :: Enemy
goomba2 = MkEnemy
    {   eType = MkEntity {entity = MkEnemyType GOOMBA, physics = initPhysics3, alive = ALIVE}
    ,   eAI = HARD
    }

initPhysics2 :: Physics
initPhysics2 = MkPhysics
    {   pos = (300.0,0.0)
    ,   vel = (200.0,300.0)
    ,   mxv = (3000,3000)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = MkHB (14*scaling) (16*scaling)    
    ,   dir = RIGHT
    }

initPhysics3 :: Physics
initPhysics3 = MkPhysics
    {   pos = (-300.0,0.0)
    ,   vel = (-200.0,300.0)
    ,   mxv = (3000,3000)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = MkHB (14*scaling) (16*scaling)    
    ,   dir = RIGHT
    }