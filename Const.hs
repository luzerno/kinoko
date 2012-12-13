module Const where
import Data.Word (Word8, Word32)
groundLevel = 600 :: Double
screenWidth = 800 :: Int
screenHeight = 600 :: Int
screenBpp = 32 :: Int
backgroundColor = (255, 0, 196) :: (Word8, Word8, Word8)

signalStep = 150 :: Int
signalColors = 3 :: Int

msPerTick = 15 :: Word32
sceneWidth = 1500 :: Int
playerScreenPosX = 300 :: Double


pointPosX = 650 ::  Int
pointPosY = 20 :: Int
fontSize = 20 :: Int
fontName = "font/Inconsolata.ttf"