module Model where

data Velocity = Vel Float Float deriving(Show,Eq)

data Point = Point Float Float deriving(Show,Eq) --Could perhaps be Int,Int but would maybe cause issues with position calc

fps :: Int
fps = 60

data Name       = Mario | Goomba    | GreenKoopa    | RedKoopa | Spiny 
    deriving (Show,Eq)
data Direction  = RIGHT | LEFT 
    deriving(Show,Eq)
data IsAlive    = ALIVE | DEAD 
    deriving(Show,Eq)
data ObjName    = Coin  | Mushroom  | FireFlower 
    deriving (Show,Eq)
data ObjType    = COIN  | POWERUP 
    deriving (Show,Eq)

-- | Data describing characters in Game (Mario & Enemies)
data Characters = Characters
    {   cName :: Name
    ,   cSpeed :: Velocity
    ,   cDirection :: Direction
    ,   cLocation :: Point
    ,   cAlive :: IsAlive
    } deriving (Show,Eq)

-- | Data descriving objects in Game (Coins & Powerups)
data Objects = Objects
    {   oName :: ObjName
    ,   oSpeed :: Velocity
    ,   oDirection :: Direction
    ,   oLocation :: Point
    ,   oAlive :: IsAlive
    ,   oType :: ObjType  
    } deriving (Show,Eq)

-- data InfoToShow = ShowNothing
--                 | ShowNumber    Int
--                 | ShowChar      Char

-- data GameState = GameState {
--                     infoToShow  :: InfoToShow,
--                     elapsedTime :: Float --Maybe could be Int
--                     }

-- initialState :: GameState
-- initialState = GameState ShowNothing 0