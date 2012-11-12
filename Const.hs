module Const where
import Data.Word


data P2 = P2 Int Int deriving (Show, Eq)
instance Num P2 where
   P2 x1 y1 + P2 x2 y2  = P2 (x1+x2) (y1+y2)
   P2 x1 y1 - P2 x2 y2  = P2 (x1-x2) (y1-y2)
   negate (P2 x y)      = P2 (-x) (-y)
   (*)                  = error "No * method for P2"
   abs                  = error "No abs method for P2"
   signum               = error "No * method for P2"
   fromInteger 0        = P2 0 0
   fromInteger _        = error "Only the constant 0 can be used as a P2"

getX :: P2 -> Int
getX (P2 x _) = x
getY :: P2 -> Int
getY (P2 _ y) = y
setX :: Int -> P2 -> P2
setX x p2 = P2 x $ getY p2
setY :: Int -> P2 -> P2
setY y p2 = P2 (getX p2) y
screenWidth     = 800 :: Int
screenHeight    = 600 :: Int
screenBpp       = 32  :: Int
sceneWidth      = 1600 :: Int
sceneHeight     = 600 :: Int
framePerSecond  = 15  :: Word32
secsPerFrame    = 1000 `div` framePerSecond :: Word32
kinokoWidth     = 78 :: Int
kinokoHeight    = 104 :: Int
walkAccel       = P2 (kinokoWidth `div` 4) 0
jumpAccel       = P2 0 (kinokoHeight `div` 2)
fallAccel       = P2 0 (kinokoHeight `div` 4)
kinokoWalkFrames    = 2 :: Int
ground          = screenHeight - kinokoHeight