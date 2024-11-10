module Model.Enemy where

import Model.Basic

data EnemyAI = EASY | MEDIUM | HARD
    deriving (Show,Eq)

instance PhysicsFunctions Enemy where
    getHitbox e = htb $ physics $ eType e
    isAlive e = alive (eType e) == ALIVE
    kill e = e {eType = (eType e) {alive = DEAD}}

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

koopa :: Enemy --edit
koopa = MkEnemy 
    {   eType = MkEntity {entity = MkEnemyType GRNKOOPA, physics = initPhysicsKoopa, alive = ALIVE}
    ,   eAI = HARD
    }

initPhysics2 :: Physics
initPhysics2 = MkPhysics
    {   pos = (300.0,0.0)
    ,   vel = (200.0,300.0)
    ,   mxv = (3000,3000)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = MkHB 14 16   
    ,   dir = RIGHT
    }

initPhysics3 :: Physics
initPhysics3 = MkPhysics
    {   pos = (-300.0,0.0)
    ,   vel = (-200.0,300.0)
    ,   mxv = (3000,3000)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = MkHB 14 16   
    ,   dir = RIGHT
    }

initPhysicsKoopa :: Physics
initPhysicsKoopa = MkPhysics
    {   pos = (-500.0,0.0)
    ,   vel = (-200.0,300.0)
    ,   mxv = (3000,3000)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = MkHB 14 16   
    ,   dir = RIGHT
    }