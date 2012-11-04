module Const where
import Data.Word
screenWidth 	= 800 :: Int
screenHeight	= 600 :: Int
screenBpp		= 32  :: Int

framePerSecond	= 15  :: Word32
secsPerFrame	= 1000 `div` framePerSecond :: Word32
kinokoWidth		= 78 :: Int
kinokoHeight	= 104 :: Int
dotAccel 		= kinokoWidth `div` 4 :: Int
kinokoWalkFrames	= 2 :: Int