module Model.Basic where
import Graphics.Gloss
import Data.Bifunctor

type Xvel = Float
type Yvel = Float
type Velocity = (Xvel,Yvel)
type Xacc = Float
type Yacc = Float
type Acceleration = (Xacc,Yacc)
data GridIndex = GRD Float Float deriving (Show,Eq)
type Width = Float
type Height = Float
data Hitbox = HB Width Height deriving (Show,Eq)

data IsGrounded = GROUNDED | AIRBORNE
    deriving (Show,Eq)
data Direction  = RIGHT | LEFT
    deriving(Show,Eq)
data IsAlive    = ALIVE | DEAD
    deriving(Show,Eq)

data Physics = Physics
    {   pos :: Point
    ,   vel :: Velocity
    ,   mxv :: Velocity
    ,   acc :: Acceleration
    ,   gnd :: IsGrounded
    ,   htb :: Hitbox
    } deriving(Show,Eq)

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
gridPos (GRD x y) = translate00 (x*blksz+(blksz/2),-(y * blksz)-(blksz/2))

translate00 :: Point -> Point
translate00 (x,y) = bimap (x -) (y +) uppbound