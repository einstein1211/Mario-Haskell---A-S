module View.Images where

import Data.ByteString as BS
import Model.Basic

data Image = MkImage
    {   hitbox :: Hitbox 
    ,   bytestring :: ByteString
    } deriving(Show,Eq)

marioStand :: Image
marioStand = MkImage
    {   hitbox = MkHB 12 16
    ,   bytestring = pack [136,112,0,255,136,112,0,255,136,112,0,255,136,112,0,255,255,255,255,0,255,255,255,0,255,255,255,0,255,255,255,0,136,112,0,255,136,112,0,255,136,112,0,255,136,112,0,255,255,255,255,0,136,112,0,255,136,112,0,255,136,112,0,255,255,255,255,0,255,255,255,0,255,255,255,0,255,255,255,0,136,112,0,255,136,112,0,255,136,112,0,255,255,255,255,0,255,255,255,0,255,255,255,0,216,40,0,255,216,40,0,255,216,40,0,255,255,255,255,0,255,255,255,0,216,40,0,255,216,40,0,255,216,40,0,255,255,255,255,0,255,255,255,0,252,152,56,255,252,152,56,255,216,40,0,255,216,40,0,255,216,40,0,255,216,40,0,255,216,40,0,255,216,40,0,255,216,40,0,255,216,40,0,255,252,152,56,255,252,152,56,255,252,152,56,255,252,152,56,255,252,152,56,255,216,40,0,255,216,40,0,255,216,40,0,255,216,40,0,255,216,40,0,255,216,40,0,255,252,152,56,255,252,152,56,255,252,152,56,255,252,152,56,255,252,152,56,255,136,112,0,255,216,40,0,255,252,152,56,255,216,40,0,255,216,40,0,255,252,152,56,255,216,40,0,255,136,112,0,255,252,152,56,255,252,152,56,255,136,112,0,255,136,112,0,255,136,112,0,255,136,112,0,255,216,40,0,255,216,40,0,255,216,40,0,255,216,40,0,255,136,112,0,255,136,112,0,255,136,112,0,255,136,112,0,255,255,255,255,0,136,112,0,255,136,112,0,255,136,112,0,255,216,40,0,255,136,112,0,255,136,112,0,255,216,40,0,255,136,112,0,255,136,112,0,255,136,112,0,255,255,255,255,0,255,255,255,0,255,255,255,0,136,112,0,255,136,112,0,255,216,40,0,255,136,112,0,255,136,112,0,255,136,112,0,255,255,255,255,0,255,255,255,0,255,255,255,0,255,255,255,0,255,255,255,0,255,255,255,0,255,255,255,0,252,152,56,255,252,152,56,255,252,152,56,255,252,152,56,255,252,152,56,255,252,152,56,255,252,152,56,255,255,255,255,0,255,255,255,0,255,255,255,0,136,112,0,255,136,112,0,255,252,152,56,255,252,152,56,255,252,152,56,255,252,152,56,255,136,112,0,255,136,112,0,255,136,112,0,255,136,112,0,255,255,255,255,0,255,255,255,0,136,112,0,255,252,152,56,255,136,112,0,255,136,112,0,255,252,152,56,255,252,152,56,255,252,152,56,255,136,112,0,255,252,152,56,255,252,152,56,255,252,152,56,255,255,255,255,0,136,112,0,255,252,152,56,255,136,112,0,255,252,152,56,255,252,152,56,255,252,152,56,255,136,112,0,255,252,152,56,255,252,152,56,255,252,152,56,255,255,255,255,0,255,255,255,0,255,255,255,0,136,112,0,255,136,112,0,255,136,112,0,255,252,152,56,255,252,152,56,255,136,112,0,255,252,152,56,255,255,255,255,0,255,255,255,0,255,255,255,0,255,255,255,0,255,255,255,0,216,40,0,255,216,40,0,255,216,40,0,255,216,40,0,255,216,40,0,255,216,40,0,255,216,40,0,255,216,40,0,255,216,40,0,255,255,255,255,0,255,255,255,0,255,255,255,0,255,255,255,0,216,40,0,255,216,40,0,255,216,40,0,255,216,40,0,255,216,40,0,255,255,255,255,0,255,255,255,0,255,255,255,0,255,255,255,0]
    }

marioJump :: Image
marioJump = MkImage
    {   hitbox = MkHB 16 16
    ,   bytestring = pack [255, 255, 255, 0, 136, 112, 0, 255, 255, 255, 255, 0, 255, 255, 255, 0, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 255, 255, 255, 0, 252, 152, 56, 255, 255, 255, 255, 0, 136, 112, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 252, 152, 56, 255, 252, 152, 56, 255, 252, 152, 56, 255, 255, 255, 255, 0, 216, 40, 0, 255, 216, 40, 0, 255, 136, 112, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 252, 152, 56, 255, 216, 40, 0, 255, 216, 40, 0, 255, 252, 152, 56, 255, 216, 40, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 252, 152, 56, 255, 252, 152, 56, 255, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 255, 255, 255, 0, 255, 255, 255, 0, 136, 112, 0, 255, 255, 255, 255, 0, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 216, 40, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 216, 40, 0, 255, 255, 255, 255, 0, 255, 255, 255, 0, 136, 112, 0, 255, 255, 255, 255, 0, 255, 255, 255, 0, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 216, 40, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 216, 40, 0, 255, 136, 112, 0, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 252, 152, 56, 255, 252, 152, 56, 255, 252, 152, 56, 255, 252, 152, 56, 255, 252, 152, 56, 255, 252, 152, 56, 255, 252, 152, 56, 255, 136, 112, 0, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 136, 112, 0, 255, 136, 112, 0, 255, 252, 152, 56, 255, 252, 152, 56, 255, 252, 152, 56, 255, 252, 152, 56, 255, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 136, 112, 0, 255, 252, 152, 56, 255, 136, 112, 0, 255, 136, 112, 0, 255, 252, 152, 56, 255, 252, 152, 56, 255, 252, 152, 56, 255, 136, 112, 0, 255, 252, 152, 56, 255, 252, 152, 56, 255, 252, 152, 56, 255, 136, 112, 0, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 136, 112, 0, 255, 252, 152, 56, 255, 136, 112, 0, 255, 252, 152, 56, 255, 252, 152, 56, 255, 252, 152, 56, 255, 136, 112, 0, 255, 252, 152, 56, 255, 252, 152, 56, 255, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 252, 152, 56, 255, 252, 152, 56, 255, 136, 112, 0, 255, 252, 152, 56, 255, 255, 255, 255, 0, 136, 112, 0, 255, 136, 112, 0, 255, 136, 112, 0, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 252, 152, 56, 255, 252, 152, 56, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 216, 40, 0, 255, 255, 255, 255, 0, 255, 255, 255, 0, 252, 152, 56, 255, 252, 152, 56, 255, 252, 152, 56, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 252, 152, 56, 255, 252, 152, 56, 255, 252, 152, 56, 255]
    }

goombaWalk1 :: Image
goombaWalk1 = MkImage
    {   hitbox = MkHB 16 16
    ,   bytestring = pack [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,0,0,0,0,0,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,255,0,0,0,255,0,0,0,255,252,188,176,255,252,188,176,255,252,188,176,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,255,0,0,0,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,0,0,0,255,0,0,0,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,0,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,252,188,176,255,252,188,176,255,252,188,176,255,200,76,12,255,200,76,12,255,252,188,176,255,252,188,176,255,252,188,176,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,252,188,176,255,0,0,0,255,252,188,176,255,200,76,12,255,200,76,12,255,252,188,176,255,0,0,0,255,252,188,176,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,0,200,76,12,255,200,76,12,255,200,76,12,255,252,188,176,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,252,188,176,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,0,0,0,0,0,200,76,12,255,200,76,12,255,200,76,12,255,252,188,176,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,252,188,176,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,0,0,0,0,0,0,0,0,0,200,76,12,255,0,0,0,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,0,0,0,255,200,76,12,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    }

goombaWalk2 :: Image
goombaWalk2 = MkImage
    {   hitbox = MkHB 16 16
    ,   bytestring = pack [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 0, 0, 0, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 0, 0, 0, 0, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 200, 76, 12, 255, 200, 76, 12, 255, 252, 188, 176, 255, 252, 188, 176, 255, 252, 188, 176, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 252, 188, 176, 255, 0, 0, 0, 255, 252, 188, 176, 255, 200, 76, 12, 255, 200, 76, 12, 255, 252, 188, 176, 255, 0, 0, 0, 255, 252, 188, 176, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 0, 0, 0, 0, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 252, 188, 176, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 252, 188, 176, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 0, 0, 0, 0, 0, 0, 0, 0, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 252, 188, 176, 255, 0, 0, 0, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 0, 0, 0, 255, 252, 188, 176, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 200, 76, 12, 255, 0, 0, 0, 255, 0, 0, 0, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 0, 0, 0, 255, 0, 0, 0, 255, 200, 76, 12, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 200, 76, 12, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    }

dirt1 :: Image
dirt1 = MkImage
    {   hitbox = MkHB 16 16
    ,   bytestring = pack [211, 83, 31, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 211, 83, 31, 255, 255, 171, 183, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 211, 83, 31, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 255, 171, 183, 255, 255, 171, 183, 255, 255, 171, 183, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 255, 171, 183, 255, 255, 171, 183, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 255, 171, 183, 255, 255, 171, 183, 255, 0, 0, 0, 255, 8, 3, 1, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 8, 3, 1, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 0, 0, 0, 255, 8, 3, 1, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 28, 11, 4, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 255, 171, 183, 255, 255, 171, 183, 255, 255, 171, 183, 255, 255, 171, 183, 255, 255, 171, 183, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 211, 83, 31, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 0, 0, 0, 255, 211, 83, 31, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 255, 171, 183, 255, 0, 0, 0, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 255, 171, 183, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 211, 83, 31, 255, 0, 0, 0, 255, 211, 83, 31, 255, 255, 171, 183, 255, 255, 171, 183, 255, 255, 171, 183, 255, 255, 171, 183, 255, 255, 171, 183, 255, 255, 171, 183, 255, 255, 171, 183, 255, 255, 171, 183, 255, 0, 0, 0, 255, 211, 83, 31, 255, 255, 171, 183, 255, 255, 171, 183, 255, 255, 171, 183, 255, 255, 171, 183, 255, 211, 83, 31, 255]
    }
stair1 :: Image
stair1 = MkImage
    {   hitbox = MkHB 16 16
    ,   bytestring = pack [0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,211,83,31,255,255,171,183,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,211,83,31,255,0,0,0,255,255,171,183,255,255,171,183,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,211,83,31,255,0,0,0,255,0,0,0,255,255,171,183,255,255,171,183,255,255,171,183,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,211,83,31,255,0,0,0,255,0,0,0,255,0,0,0,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,211,83,31,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,255,171,183,255,255,171,183,255,255,171,183,255,211,83,31,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,255,171,183,255,255,171,183,255,211,83,31,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,0,0,0,255,0,0,0,255,0,0,0,255,255,171,183,255,211,83,31,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,0,0,0,255,0,0,0,255,211,83,31,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,255,171,183,255,0,0,0,255]
    }
pipe_tl1 :: Image
pipe_tl1 = MkImage
    {   hitbox = MkHB 16 16
    ,   bytestring = pack [0,0,0,0,0,0,0,0,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,140,0,255,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,140,0,255,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,140,0,255,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,140,0,255,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,140,0,255,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,140,0,255,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,140,0,255,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,140,0,255,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,140,0,255,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,140,0,255,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,140,0,255,0,0,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255]
    }
pipe_tr1 :: Image
pipe_tr1 = MkImage
    {   hitbox = MkHB 16 16
    ,   bytestring = pack [0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,0,0,0,0,0,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,0,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,0,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,0,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,0,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,0,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255]
    }
pipe_l1 :: Image
pipe_l1 = MkImage
    {   hitbox = MkHB 16 16
    ,   bytestring = pack [0,0,0,0,0,0,0,0,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,0,0,0,0,0,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,0,0,0,0,0,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,0,0,0,0,0,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,0,0,0,0,0,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,0,0,0,0,0,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,0,0,0,0,0,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,0,0,0,0,0,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,0,0,0,0,0,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,0,0,0,0,0,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,0,0,0,0,0,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,0,0,0,0,0,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,0,0,0,0,0,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,0,0,0,0,0,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,0,0,0,0,0,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,0,0,0,0,0,0,0,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255]
    }
pipe_r1 :: Image
pipe_r1 = MkImage
    {   hitbox = MkHB 16 16
    ,   bytestring = pack [170,210,15,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,255,0,0,0,0,0,0,0,0,170,210,15,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,0,0,255,0,0,0,0,0,0,0,0,170,210,15,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,255,0,0,0,0,0,0,0,0,170,210,15,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,0,0,255,0,0,0,0,0,0,0,0,170,210,15,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,255,0,0,0,0,0,0,0,0,170,210,15,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,0,0,255,0,0,0,0,0,0,0,0,170,210,15,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,255,0,0,0,0,0,0,0,0,170,210,15,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,0,0,255,0,0,0,0,0,0,0,0,170,210,15,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,255,0,0,0,0,0,0,0,0,170,210,15,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,0,0,255,0,0,0,0,0,0,0,0,170,210,15,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,255,0,0,0,0,0,0,0,0,170,210,15,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,0,0,255,0,0,0,0,0,0,0,0,170,210,15,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,255,0,0,0,0,0,0,0,0,170,210,15,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,0,0,255,0,0,0,0,0,0,0,0,170,210,15,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,0,0,0,255,0,0,0,0,0,0,0,0,170,210,15,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,0,140,0,255,170,210,15,255,0,140,0,255,170,210,15,255,170,210,15,255,170,210,15,255,0,0,0,255,0,0,0,0,0,0,0,0]
    }

brick1 :: Image
brick1 = Img
    {   hitbox = HB 16 16
    ,   bytestring = pack [0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,200,76,12,255,0,0,0,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255,252,188,176,255]
    }