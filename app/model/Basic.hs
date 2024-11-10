{-# LANGUAGE InstanceSigs #-}
module Model.Basic where
import Graphics.Gloss

-- Basic types for entities in the game
type Xvel = Float
type Yvel = Float
type Velocity = (Xvel,Yvel)
type Xacc = Float
type Yacc = Float
type Acceleration = (Xacc,Yacc)
type Width = Float
type Height = Float
data Hitbox = MkHB Width Height deriving (Show,Eq)
type Resolution = (Int,Int)
type Scaling = Float 

data IsGrounded = GROUNDED | AIRBORNE
    deriving (Show,Eq)
data Direction  = RIGHT | LEFT
    deriving(Show,Eq)

data Entry        = PlayerEntry | EnemyEntry | ItemEntry | BlockEntry | PlatformEntry
    deriving (Enum)
data PlayerType   = MARIO | LUIGI
    deriving (Show,Eq)
data EnemyType   = GOOMBA| GRNKOOPA  | REDKOOPA | SPINY | PIRANHA
    deriving (Show,Eq)
data ItemType    = COIN  | HIDDENCOIN| MUSHROOM | FIREFLOWER | STAR 
    deriving (Show,Eq)
data BlockType   = BRICK | QBLOCK    | EMPTYBLOCK | HIDDENBLOCK
    deriving (Show,Eq)
data EntityType = MkPlayerType PlayerType | MkEnemyType EnemyType | MkItemType ItemType | MkBlockType BlockType
    deriving (Show,Eq)
data Alive       = ALIVE | DEAD
    deriving (Show,Eq)

-- Physics functions class to extract and modify several parameters
class PhysicsFunctions f where
    getHitbox :: f -> Hitbox
    getPos    :: f -> Point
    getVel    :: f -> Velocity
    getAcc    :: f -> Acceleration
    getDir    :: f -> Direction
    isAlive   :: f -> Bool
    moveBy    :: (Float,Float) -> f -> f
    kill      :: f -> f
    isGrounded :: f -> Bool

-- Physics datatype that holds the parameters for physics calculation
data Physics = MkPhysics
    {   pos :: Point
    ,   vel :: Velocity
    ,   mxv :: Velocity
    ,   acc :: Acceleration
    ,   gnd :: IsGrounded
    ,   htb :: Hitbox
    ,   dir :: Direction
    } deriving (Show,Eq)

-- Entity datatype that holds physics, alive status and the entity type marker
data Entity = MkEntity
    {   entity :: EntityType
    ,   physics :: Physics
    ,   alive :: Alive
    } deriving (Show,Eq)

instance PhysicsFunctions Entity where
    kill :: Entity -> Entity
    kill e = e {alive = DEAD}

-- Fps the game will run at
fps :: Int
fps = 100

-- Resolution the game will be set at
res :: Resolution
res = (1024,768)  --16 blocks wide, 12 blocks high

-- Ratio the entities need to be scaled up to at the start of the Game
startScaling :: Float
startScaling = 1

-- Blocksize calculation with scaling
blksz :: Scaling -> Float
blksz s = 64*s

-- Function that calculates the point location of a gridlocation on the screen
makeGridPos :: (Int,Int) -> Scaling -> Point
makeGridPos (x,y) s = translate00 (fromIntegral x*blk+(blk/2),-(fromIntegral y*blk)-(blk/2)) s
  where
    blk = blksz s

-- Helper function for gridposition function
translate00 :: Point -> Scaling -> Point
translate00 (x,y) s = (x-(fst uppbound *s),y+(snd uppbound *s))

-- Upper and Right boundaries of screen
uppbound :: (Float,Float)
uppbound = (fromIntegral (fst res) / 2, fromIntegral (snd res) / 2)

-- Lower and Left boundaries of screen
lowbound :: (Float,Float)
lowbound = (-fst uppbound,-snd uppbound)