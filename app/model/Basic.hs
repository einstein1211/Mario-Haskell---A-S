module Model.Basic where
import Graphics.Gloss


type Xvel = Float
type Yvel = Float
type Velocity = (Xvel,Yvel)
type Xacc = Float
type Yacc = Float
type Acceleration = (Xacc,Yacc)
data GridIndex = MkGrid Int Int deriving (Show,Eq)
type Width = Float
type Height = Float
data Hitbox = MkHB Width Height deriving (Show,Eq)
type Resolution = (Int,Int)
type Scaling = Float 

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

class PhysicsFunctions f where
    getHitbox :: f -> Hitbox
    getPos    :: f -> Point
    getVel    :: f -> Velocity
    getAcc    :: f -> Acceleration
    isAlive   :: f -> Bool
    moveBy    :: (Float,Float) -> f -> f

-- class GridIndexFunctions a where
--     changeGridIndex :: GridIndex -> a -> a
--     getGridIndex :: a -> GridIndex

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

res :: Resolution
res = (1024,768)  --16 blocks wide, 12 blocks high

startScaling :: Float
startScaling = 1

blksz :: Scaling -> Float
blksz s = 64*s

gridPos :: GridIndex -> Scaling -> Point
gridPos (MkGrid x y) s = translate00 (fromIntegral x*blk+(blk/2),-(fromIntegral y*blk)-(blk/2)) s
  where
    blk = blksz s

makeGridPos :: (Int,Int) -> Scaling -> Point
makeGridPos (x,y) s = translate00 (fromIntegral x*blk+(blk/2),-(fromIntegral y*blk)-(blk/2)) s
  where
    blk = blksz s

translate00 :: Point -> Scaling -> Point
translate00 (x,y) s = (x-(fst uppbound *s),y+(snd uppbound *s))

uppbound :: (Float,Float)
uppbound = (fromIntegral (fst res) / 2, fromIntegral (snd res) / 2)

lowbound :: (Float,Float)
lowbound = (-fst uppbound,-snd uppbound)