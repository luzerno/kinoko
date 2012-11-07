{-# LANGUAGE FlexibleContexts #-}

module Main where
import Data.Word
import Data.Array.IArray
import Data.List

import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.Utilities

import Foreign
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import Util
import Const
import Game
import Timer
import Kinoko


foreign import ccall unsafe "SDL_GetKeyState" sdlGetKeyState :: Ptr CInt -> IO (Ptr Word8)
getKeyState :: IO [SDLKey]
getKeyState = alloca $ \numkeysPtr -> do
  keysPtr <- sdlGetKeyState numkeysPtr
  numkeys <- peek numkeysPtr
  (map Graphics.UI.SDL.Utilities.toEnum . map fromIntegral . findIndices (== 1)) `fmap` peekArray (fromIntegral numkeys) keysPtr




handleInput :: Event -> Kinoko -> Kinoko

-- walk
handleInput (KeyDown (Keysym SDLK_LEFT _ _)) n@Kinoko {velocity = velocity} = n {velocity = velocity - walkAccel}
handleInput (KeyDown (Keysym SDLK_RIGHT _ _)) n@Kinoko {velocity = velocity} = n {velocity = velocity + walkAccel}
handleInput (KeyUp (Keysym SDLK_LEFT _ _)) n@Kinoko {velocity = velocity} = n {velocity = velocity + walkAccel}
handleInput (KeyUp (Keysym SDLK_RIGHT _ _)) n@Kinoko {velocity = velocity} = n {velocity = velocity - walkAccel}
-- jump
handleInput (KeyDown (Keysym SDLK_UP _ _)) n@Kinoko {velocity = velocity, stand = stand} = 
										if stand 
											then n {velocity = velocity - jumpAccel, stand = False}
											else n {velocity = velocity}
--handleInput (KeyUp (Keysym SDLK_UP _ _)) n@Kinoko {velocity = velocity} = n {velocity = velocity - jumpAccel}
handleInput (KeyDown (Keysym SDLK_DOWN _ _)) n@Kinoko {velocity = velocity} = n {velocity = velocity - jumpAccel}
handleInput (KeyUp (Keysym SDLK_DOWN _ _)) n@Kinoko {velocity = velocity} = n {velocity = velocity + jumpAccel}
handleInput _ d = d




initEnv :: IO (AppConfig, AppData)
initEnv = do
	screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
	setCaption "Kinoko Test" []
	kinoko <- loadImage "nikki.png" (Just (0xff, 0xff, 0xff))
	fps <- start defaultTimer
	return (AppConfig screen kinoko, AppData walk camera fps)
   where walk = defaultKinoko
         camera = Rect 0 0 screenWidth screenHeight



loop :: [SDLKey] -> AppEnv ()
loop ks = do
	modifyFPSM $ liftIO . start
	quit <- whileEvents $ modifyKinoko . handleInput
	modifyKinoko $ updateKinoko . move

	fps <- getFPS
	walk <- getKinoko
	kinokoSprite <- getKinokoSprite
	screen <- getScreen
	liftIO $ do
		jrect <- Just `liftM` getClipRect screen
		white <- mapRGB' screen 0xff 0xff 0xff
		black <- mapRGB' screen 0x00 0x00 0x00
		fillRect screen jrect white
		showKinoko walk kinokoSprite screen clipsLeft clipsRight

		Graphics.UI.SDL.flip screen

		ticks <- getTimerTicks fps
		when (ticks < secsPerFrame) $ do
			delay $ secsPerFrame - ticks

	unless quit $ loop ks
   where
	mapRGB' = mapRGB . surfaceGetPixelFormat
	clipsRight = listArray (0, 2) [Rect 0 0 kinokoWidth kinokoHeight, Rect kinokoWidth 0 kinokoWidth kinokoHeight,
								   Rect (kinokoWidth * 2) 0 kinokoWidth kinokoHeight] :: Array Int Rect
	clipsLeft = listArray (0, 2) [Rect 0 kinokoHeight kinokoWidth kinokoHeight, Rect kinokoWidth kinokoHeight kinokoWidth kinokoHeight,
								  Rect (kinokoWidth * 2) kinokoHeight kinokoWidth kinokoHeight] :: Array Int Rect


whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
	event <- liftIO pollEvent
	case event of
		Quit -> return True
		NoEvent -> return False
		_       ->  do
			act event
			whileEvents act

runLoop :: [SDLKey] -> AppConfig -> AppData -> IO ()
runLoop ks = evalStateT . runReaderT (loop ks)

main = withInit [InitEverything] $ do -- withInit calls quit for us.
	(env, state) <- initEnv
	ks <- getKeyState
	runLoop ks env state