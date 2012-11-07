{-# LANGUAGE FlexibleContexts #-}

module Game where
import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Control.Monad.State
import Control.Monad.Reader

import Timer

data Kinoko = Kinoko { pos :: P2, velocity :: P2, frame :: Int, status :: KinokoDir}
data KinokoDir = MoveLeft | MoveRight deriving (Show, Enum, Bounded, Eq, Ord)
defaultKinoko = Kinoko (P2 0 0) (P2 0 0) 0 MoveRight
type Camera = Rect

data AppData = AppData {
	kinoko :: Kinoko,
	camera :: Camera,
	fps :: Timer
}
data AppConfig = AppConfig {
	screen :: Surface,
	kinokoSprite :: Surface
}
type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState

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