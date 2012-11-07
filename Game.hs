{-# LANGUAGE FlexibleContexts #-}

module Game where
import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Control.Monad.State
import Control.Monad.Reader

import Timer
import Const
data Kinoko = Kinoko { pos :: P2,
                       velocity :: P2, 
                       stand :: Bool,
                       frame :: Int, 
                       status :: KinokoDir }
data KinokoDir = MoveLeft | MoveRight deriving (Show, Enum, Bounded, Eq, Ord)
--defaultKinoko = Kinoko (P2 0 0) (P2 0 0) 0 MoveRight
defaultKinoko = Kinoko {
                     pos = P2 0 ground,
                     velocity = P2 0 0,
                     stand = True,
                     frame = 0,
                     status = MoveRight
               }
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

