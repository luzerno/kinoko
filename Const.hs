module Const where
import Data.Word
import Game
screenWidth 	= 800 :: Int
screenHeight	= 600 :: Int
screenBpp		= 32  :: Int
sceneWidth		= 1600 :: Int
sceneHeight		= 600 :: Int
framePerSecond	= 15  :: Word32
secsPerFrame	= 1000 `div` framePerSecond :: Word32
kinokoWidth		= 78 :: Int
kinokoHeight	= 104 :: Int
dotAccel 		= P2 (kinokoWidth `div` 4) 0
kinokoWalkFrames	= 2 :: Int