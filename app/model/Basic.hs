module Model.Basic where
import Graphics.Gloss
import Data.Bifunctor

type Xvel = Float
type Yvel = Float
type Velocity = (Xvel,Yvel)
type Xacc = Float
type Yacc = Float
type Acceleration = (Xacc,Yacc)
data GridIndex = MkGrid Float Float deriving (Show,Eq)
type Width = Float
type Height = Float
data Hitbox = MkHB Width Height deriving (Show,Eq)


data IsGrounded = GROUNDED | AIRBORNE
    deriving (Show,Eq)
data Direction  = RIGHT | LEFT
    deriving(Show,Eq)

data PlayerType   = MARIO | LUIGI
    deriving (Show,Eq)
data EnemyType   = GOOMBA| GRNKOOPA  | REDKOOPA | SPINY | PIRANHA
    deriving (Show,Eq)
data ItemType    = COIN  | HIDDENCOIN| MUSHROOM | FIREFLOWER | STAR 
    deriving (Show,Eq)
data BlockType   = BRICK | QBLOCK     | EMPTYBLOCK | HIDDENBLOCK
    deriving (Show,Eq)
data EntityType = MkPlayerType PlayerType | MkEnemyType EnemyType | MkItemType ItemType | MkBlockType BlockType
    deriving (Show,Eq)
data Alive       = ALIVE | DEAD
    deriving (Show,Eq)

class IsAlive a where 
  isAlive :: a -> Bool 

class GetHitbox a where
    getHitbox :: a -> Hitbox

data Physics = MkPhysics
    {   pos :: Point
    ,   vel :: Velocity
    ,   mxv :: Velocity
    ,   acc :: Acceleration
    ,   gnd :: IsGrounded
    ,   htb :: Hitbox
    ,   dir :: Direction
    } deriving (Show,Eq)

data Entity = MkEntity
    {   entity :: EntityType
    ,   physics :: Physics
    ,   alive :: Alive
    } deriving (Show,Eq)

fps :: Int
fps = 100

res :: (Int,Int)
res = (1024,768) --16 blocks wide, 12 blocks high

scaling :: Float
scaling = 4

uppbound :: (Float,Float)
uppbound = (fromIntegral (fst res) / 2, fromIntegral (snd res) / 2)

lowbound :: (Float,Float)
lowbound = (-fst uppbound,-snd uppbound)
-- lowbound = (fromIntegral (-fst res) / 2, fromIntegral (-snd res) / 2)

blksz :: Float
blksz = 16*scaling

gridPos :: GridIndex -> Point
gridPos (MkGrid x y) = translate00 (x*blksz+(blksz/2),-(y * blksz)-(blksz/2))

translate00 :: Point -> Point
translate00 (x,y) = bimap (x -) (y +) uppbound