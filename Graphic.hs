module Graphic where
import Graphics.UI.SDL hiding (Event)
import Graphics.UI.SDL hiding (Event)
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.Rotozoomer
import Graphics.UI.SDL.TTF
import Data.Word (Word32, Word8)
import GameTypes
import Const
import Control.Monad.State
import Control.Monad


getScreen :: IO Surface
getScreen = do
  screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
  setCaption "Kinoko Test" []
  return screen

loadSingleImage :: Maybe (Word8, Word8, Word8) -> String -> IO Surface
loadSingleImage colorKey filename = load filename >>= displayFormat >>= setColorKey' colorKey

loadImages :: [String] -> IO [Surface]
loadImages [] = return []
loadImages (x : xs) = do
			a <- loadSingleImage (Just backgroundColor) x
			b <- loadImages xs
			return (a : b)

applySurface :: Int -> Int -> Surface -> Surface -> IO Bool
applySurface x y src dst = blitSurface src Nothing dst offset
  where offset = Just Rect {rectX = x, rectY = y, rectW = 0, rectH = 0} 

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (mapRGB . surfaceGetPixelFormat) surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

drawPoint :: Surface -> Font -> Actor -> Int -> StateT Env IO ()
drawPoint screen font player bonusNumber = do
	--player <- getPlayerActor
	liftIO $ putPointToScreen screen font (point player) bonusNumber

putPointToScreen :: Surface -> Font -> Int -> Int -> IO ()
putPointToScreen screen font p bonusNumber = do
	message <- renderTextSolid font ("Points: " ++ (show p) ++ "/" ++ (show bonusNumber)) (Color 255 255 255)
	applySurface pointPosX pointPosY message screen
	return ()


getPlayerScreenPosition :: P2 -> Double -> Double -> P2
getPlayerScreenPosition (P2 px py) w h
	| px <= playerScreenPosX = P2 px py
	| px >= fromIntegral sceneWidth - fromIntegral screenWidth + playerScreenPosX = P2 (fromIntegral screenWidth - fromIntegral sceneWidth + px) py
	| otherwise = P2 playerScreenPosX py

drawActors :: Surface -> Actor -> StateT Env IO ()
drawActors screen player = do
	--player <- getPlayerActor
	(actors, cnt) <- get
	let pos = positionS player
	let newPos = getPlayerScreenPosition pos (width player) (height player)
	let d = newPos - pos
	liftIO $ putActorsToScreen screen (map snd actors) d

draw :: Surface -> Font -> Actor -> Int -> StateT Env IO ()
draw screen font player bonusNumber = do 
		  rect <- liftIO $ getClipRect screen
		  black <- liftIO $ mapRGB (surfaceGetPixelFormat screen) 0x00 0x00 0x00
		  white <- liftIO $ mapRGB (surfaceGetPixelFormat screen) 0xFF 0xFF 0xFF
		  liftIO $ fillRect screen (Just rect) black 
		  drawActors screen player
		  drawPoint screen font player bonusNumber
		  liftIO $ Graphics.UI.SDL.flip screen
		    --delay 1
		  return ()

putActorsToScreen :: Surface -> [Actor] -> P2 -> IO ()
putActorsToScreen screen [] _ = return ()
putActorsToScreen screen (a:as) d = do 
	when (length (image a) > 0) $ do
		let (P2 x y) = positionS a + d
		let dir = direction a
		let P2 _ accelY = accel a
		sur <- case (label a) of
			Player -> do
				let frame = ((stepS a) `div` 10) `mod` ((length $ surface a) - 1)
				let (P2 _ accelY) = accel a
				let sur = if accelY /= 0
					then last $ surface a
					else if velocityS a == P2 0 0 && accel a == P2 0 0 
						then head $ surface a
						else surface a !! frame
				if direction a == RightDir
					then return $ sur
					else zoom sur (-1) (1) True
			Signal -> do
				let color = ((stepS a) `div` signalStep) `mod` (length $ surface a)
				return $ surface a !! color
			otherwise -> return $ head $ surface a
		applySurface (round x) (round y) sur screen 
		return ()
	putActorsToScreen screen as d
