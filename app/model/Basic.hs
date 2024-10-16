module Model.Basic where

import Graphics.Gloss

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
    