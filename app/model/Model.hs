module Model.Model where

import Model.Basic
import Model.Platforms

import Graphics.Gloss.Interface.IO.Game (SpecialKey)

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
data Movement   = NORMAL| RUNNING   | CROUCHED
    deriving (Show,Eq)
data Status     = SMALL | BIG       | FIRE
    deriving (Show,Eq)
data HasWon     = WON   | LOST      | PLAYING
    deriving(Show,Eq)

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
    ,   platforms = [pipe1,pipe2,pipe3,pipe4] ++ makeFloor --TODO: replace with mapped column list
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