module Model.Enemy where

import Model.Basic
import Graphics.Gloss

data EnemyAI = EASY | MEDIUM | HARD
    deriving (Show,Eq)

instance PhysicsFunctions Enemy where
    getPos e = pos $ physics $ eType e
    getVel e = vel $ physics $ eType e
    getDir e = dir $ physics $ eType e
    getHitbox e = htb $ physics $ eType e
    isAlive e = alive (eType e) == ALIVE
    moveBy (xoff,yoff) e = e{eType = (eType e) {physics = (physics (eType e)) {pos = (xoff+fst(getPos e),yoff+snd(getPos e))}}}
    kill e = e {eType = (eType e) {alive = DEAD}}

-- | Data describing enemies in Game 
data Enemy = MkEnemy
    {   eType :: Entity
    ,   eAI :: EnemyAI
    } deriving (Show,Eq)

makeGoomba :: Point -> Enemy
makeGoomba (x,y) = MkEnemy
    {   eType = MkEntity 
                {entity = MkEnemyType GOOMBA, 
                physics = goombaPhys {pos = (x,y)}, 
                alive = ALIVE}
    ,   eAI = EASY
    }

goomba :: Enemy
goomba = MkEnemy
    {   eType = MkEntity {entity = MkEnemyType GOOMBA, physics = goombaPhys, alive = ALIVE}
    ,   eAI = EASY
    }

goomba2 :: Enemy
goomba2 = MkEnemy
    {   eType = MkEntity {entity = MkEnemyType GOOMBA, physics = initPhysics3, alive = ALIVE}
    ,   eAI = HARD
    }

koopa :: Enemy --edit
koopa = MkEnemy 
    {   eType = MkEntity {entity = MkEnemyType GRNKOOPA, physics = initPhysics4, alive = ALIVE}
    ,   eAI = HARD
    }

goombaPhys :: Physics
goombaPhys = MkPhysics
    {   pos = (300.0,0.0)
    ,   vel = (-150.0,0.0)
    ,   mxv = (3000,3000)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = MkHB 14 16   
    ,   dir = LEFT
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

initPhysics4 :: Physics
initPhysics4 = MkPhysics
    {   pos = (-500.0,0.0)
    ,   vel = (-200.0,300.0)
    ,   mxv = (3000,3000)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = MkHB 14 16   
    ,   dir = RIGHT
    }