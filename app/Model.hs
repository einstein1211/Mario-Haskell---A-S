module Model where

data Point = Pnt Float Float deriving(Show,Eq) --Could perhaps be Int,Int but would maybe cause issues with position calc
data Velocity = Vel Float Float deriving(Show,Eq)
data Acceleration = Acc Float Float deriving(Show,Eq)
data GridIndex = Grd Int Int deriving (Show,Eq)
data Hitbox = HB Point Point deriving (Show,Eq)

fps :: Int
fps = 60

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
    {   position :: Point
    ,   velocity :: Velocity
    ,   acceleration :: Acceleration
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
    ,   players = [Mario]
    ,   enemies = [GOOMBA,GOOMBA]
    ,   items = []
    ,   blocks = []
    ,   platforms = []
    }