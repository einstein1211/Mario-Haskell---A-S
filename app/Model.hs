module Model where

import Graphics.Gloss

type Xvel = Float 
type Yvel = Float
type Velocity = (Xvel,Yvel)
type Xacc = Float
type Yacc = Float
type Acceleration = (Xacc,Yacc)
data GridIndex = Grd Int Int deriving (Show,Eq)
data Hitbox = HB Point Point deriving (Show,Eq)

fps :: Int
fps = 100

data PlyrType   = Mario | Luigi
    deriving (Show,Eq)
data EnmyType   = GOOMBA| GRNKOOPA  | REDKOOPA | SPINY | PIRANHA
    deriving (Show,Eq)
data EnmyAI     = EASY  | MEDIUM    | HARD
    deriving (Show,Eq)
data ItmType    = COIN  | HIDDENCOIN| MUSHROOM | FIREFLOWER | STAR 
    deriving (Show,Eq)
data BlckType   = BRICK | BLOCK     | EMPTYBLOCK
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
    ,   acc :: Acceleration
    } deriving(Show,Eq)

-- | Data describing players in Game 
data Player = Player
    {   plyType :: PlyrType
    ,   plyPhysics :: Physics
    ,   plyDirection :: Direction
    ,   plyAlive :: IsAlive
    ,   plyGrounded :: IsGrounded
    ,   plyMovement :: Movement
    ,   plyPower :: Status
    } deriving (Show,Eq)

-- | Data describing enemies in Game 
data Enemy = Enemy
    {   eType :: EnmyType
    ,   ePhysics :: Physics
    ,   eDirection :: Direction
    ,   eAlive :: IsAlive
    ,   eGrounded :: IsGrounded
    ,   eAI :: EnmyAI
    } deriving (Show,Eq)

-- | Data descriving objects in Game (Coins & Powerups)
data Item = NOITEM | Item
    {   iType :: ItmType
    ,   iHitbox :: Hitbox
    ,   iPhysics :: Physics
    ,   iAlive :: IsAlive
    ,   iGrounded :: IsGrounded
    } deriving (Show,Eq)

data Block = Block
    {   bType :: BlckType
    ,   bHitbox :: Hitbox
    ,   bPosition :: GridIndex
    ,   bAlive :: IsAlive
    ,   bContents :: Item
    } deriving (Show,Eq)

data Platform = Platform
    {   pltHitbox :: Hitbox
    ,   pltPosition :: GridIndex
    } deriving (Show,Eq)



-- data InfoToShow = ShowNothing
--                 | ShowNumber    Int
--                 | ShowChar      Char

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
    } deriving (Show,Eq)

initialState :: GameState
initialState = GameState
    {   lives = 10
    ,   score = 0
    ,   time = 0.0
    ,   status = PLAYING
    ,   players = [mario]
    ,   enemies = [goomba,goomba]
    ,   items = []
    ,   blocks = []
    ,   platforms = []
    }

mario :: Player
mario = Player
    {   plyType = Mario
    ,   plyPhysics = initPhysics
    ,   plyDirection = RIGHT
    ,   plyAlive = ALIVE
    ,   plyGrounded = GROUNDED
    ,   plyMovement = NORMAL
    ,   plyPower = SMALL
    }

marioPath :: Path
marioPath = [(0.0,0.0),(40.0,0.0),(40.0,40.0),(0.0,40.0)]

goomba :: Enemy
goomba = Enemy
    {   eType = GOOMBA
    ,   ePhysics = initPhysics2
    ,   eDirection = RIGHT
    ,   eAlive = ALIVE
    ,   eGrounded = GROUNDED
    ,   eAI = EASY
    }

initPhysics :: Physics
initPhysics = Physics
    {   pos = (0.0,0.0)
    ,   vel = (0.0,0.0)
    ,   acc = (100.0,10.0)
    }

initPhysics2 :: Physics
initPhysics2 = Physics
    {   pos = (0.0,0.0)
    ,   vel = (0.0,0.0)
    ,   acc = (-100.0,10.0)
    }