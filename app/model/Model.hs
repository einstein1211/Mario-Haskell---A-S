module Model.Model where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (SpecialKey)

type Xvel = Float 
type Yvel = Float
type Velocity = (Xvel,Yvel)
type Xacc = Float
type Yacc = Float
type Acceleration = (Xacc,Yacc)
data GridIndex = Grd Int Int deriving (Show,Eq)
type Width = Float
type Height = Float
data Hitbox = HB Width Height deriving (Show,Eq)

fps :: Int
fps = 100

res :: (Int,Int)
res = (1280,720)

scaling :: Float
scaling = 8

uppbound :: (Float,Float)
uppbound = (fromIntegral (fst res) / 2, fromIntegral (snd res) / 2)

lowbound :: (Float,Float)
lowbound = (-fst uppbound,-snd uppbound)
-- lowbound = (fromIntegral (-fst res) / 2, fromIntegral (-snd res) / 2)

data PlyrType   = MARIO | LUIGI
    deriving (Show,Eq)
data EnmyType   = GOOMBA| GRNKOOPA  | REDKOOPA | SPINY | PIRANHA
    deriving (Show,Eq)
data EnmyAI     = EASY  | MEDIUM    | HARD
    deriving (Show,Eq)
data ItmType    = COIN  | HIDDENCOIN| MUSHROOM | FIREFLOWER | STAR 
    deriving (Show,Eq)
data BlckType   = BRICK | BLOCK     | EMPTYBLOCK
    deriving (Show,Eq)
data PltType    = DIRT  | STAIR     | PIPEL    | PIPER  | PIPETL  | PIPETR
    deriving (Show,Eq)
data Movement   = NORMAL| RUNNING   | CROUCHED
    deriving (Show,Eq)
data IsGrounded = GROUNDED | AIRBORNE
    deriving (Show,Eq)
data Status     = SMALL | BIG       | FIRE
    deriving (Show,Eq)
data Direction  = RIGHT | LEFT 
    deriving(Show,Eq)
data IsAlive    = ALIVE | DEAD 
    deriving(Show,Eq)
data HasWon     = WON   | LOST      | PLAYING
    deriving(Show,Eq)

data Physics = Physics
    {   pos :: Point
    ,   vel :: Velocity
    ,   mxv :: Velocity
    ,   acc :: Acceleration
    ,   gnd :: IsGrounded
    ,   htb :: Hitbox
    } deriving(Show,Eq)

-- | Data describing players in Game 
data Player = Player
    {   plyType :: PlyrType
    ,   plyPhysics :: Physics
    ,   plyDirection :: Direction
    ,   plyAlive :: IsAlive
    ,   plyMovement :: Movement
    ,   plyPower :: Status
    } deriving (Show,Eq)

-- | Data describing enemies in Game 
data Enemy = Enemy
    {   eType :: EnmyType
    ,   ePhysics :: Physics
    ,   eDirection :: Direction
    ,   eAlive :: IsAlive
    ,   eAI :: EnmyAI
    } deriving (Show,Eq)

-- | Data descriving objects in Game (Coins & Powerups)
data Item = NOITEM | Item
    {   iType :: ItmType
    ,   iPhysics :: Physics
    ,   iAlive :: IsAlive
    } deriving (Show,Eq)

data Block = Block
    {   bType :: BlckType
    ,   bHitbox :: Hitbox
    ,   bPosition :: GridIndex
    ,   bAlive :: IsAlive
    ,   bContents :: Item
    } deriving (Show,Eq)

data Platform = Platform
    {   pltType :: 
    ,   pltHitbox :: Hitbox
    ,   pltPosition :: GridIndex 
    } deriving (Show,Eq)

data GameState = GameState 
    {   lives :: Int
    ,   score :: Int
    ,   time :: Float --Maybe could be Int
    ,   status :: HasWon
    ,   players :: [Player]
    ,   enemies :: [Enemy]
    ,   items :: [Item]
    ,   blocks :: [Block]
    ,   platforms :: [Platform]
    ,   pressedKeys :: [SpecialKey]
    } deriving (Show,Eq)

initialState :: GameState
initialState = GameState
    {   lives = 10
    ,   score = 0
    ,   time = 0.0
    ,   status = PLAYING
    ,   players = [mario]
    ,   enemies = [goomba,goomba2]
    ,   items = []
    ,   blocks = []
    ,   platforms = [] --TODO: replace with mapped column list
    ,   pressedKeys = []
    }

mario :: Player
mario = Player
    {   plyType = MARIO
    ,   plyPhysics = initPhysics
    ,   plyDirection = RIGHT
    ,   plyAlive = ALIVE
    ,   plyMovement = NORMAL
    ,   plyPower = SMALL
    }

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

initPhysics :: Physics
initPhysics = Physics
    {   pos = (0.0,0.0)
    ,   vel = (0.0,3000.0)
    ,   mxv = (500,500)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = HB 12 16
    }

initPhysics2 :: Physics
initPhysics2 = Physics
    {   pos = (0.0,0.0)
    ,   vel = (400.0,300.0)
    ,   mxv = (3000,3000)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = HB 16 16    
    }

initPhysics3 :: Physics
initPhysics3 = Physics
    {   pos = (0.0,0.0)
    ,   vel = (-400.0,300.0)
    ,   mxv = (3000,3000)
    ,   acc = (0.0,0.0)
    ,   gnd = AIRBORNE
    ,   htb = HB 16 16    
    }