{-# LANGUAGE FlexibleContexts #-}

module Game where
import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Control.Monad.State
import Control.Monad.Reader

import Timer

data Kinoko = Kinoko { offset :: Int, velocity :: Int, frame :: Int, status :: KinokoDir}
data KinokoDir = MoveLeft | MoveRight deriving (Show, Enum, Bounded, Eq, Ord)
defaultKinoko = Kinoko 0 0 0 MoveRight
data AppData = AppData {
	kinoko :: Kinoko,
	fps :: Timer
}
data AppConfig = AppConfig {
	screen :: Surface,
	kinokoSprite :: Surface
}
type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState