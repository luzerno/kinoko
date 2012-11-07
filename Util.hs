{-# LANGUAGE FlexibleContexts #-}

module Util where
import Data.Word
import Data.Array.IArray
import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Control.Monad.State
import Control.Monad.Reader
import Game
import Timer
import Const

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }



getFPS :: MonadState AppData m => m Timer
getFPS = liftM fps get

putFPS :: MonadState AppData m => Timer -> m ()
putFPS t = modify $ \s -> s { fps = t }

modifyFPSM :: MonadState AppData m => (Timer -> m Timer) -> m ()
modifyFPSM act = getFPS >>= act >>= putFPS

getKinoko :: MonadState AppData m => m Kinoko
getKinoko = liftM kinoko get

putKinoko :: MonadState AppData m => Kinoko -> m ()
putKinoko n = modify $ \s -> s {kinoko = n}

modifyKinokoM :: MonadState AppData m => (Kinoko -> m Kinoko) -> m ()
modifyKinokoM act = getKinoko >>= act >>= putKinoko

modifyKinoko :: MonadState AppData m => (Kinoko -> Kinoko) -> m ()
modifyKinoko fn = fn `liftM` getKinoko >>= putKinoko

getScreen :: MonadReader AppConfig m => m Surface
getScreen = liftM screen ask

getKinokoSprite :: MonadReader AppConfig m => m Surface
getKinokoSprite = liftM kinokoSprite ask


setCamera :: Kinoko -> Camera -> Camera
setCamera Kinoko {pos = P2 x y} rect@(Rect _ _ w h) = rect {rectX = x'', rectY = y''}
    where
        x' = (x + kinokoWidth `div` 2) - screenWidth `div` 2
        y' = (y + kinokoHeight `div` 2) - screenHeight `div` 2
        x'' = min (sceneWidth - w) $ max x' 0
        y'' = min (sceneHeight - h) $ max y' 0


