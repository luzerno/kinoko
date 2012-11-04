module Kinoko where
import Data.Array.IArray

import Util
import Const
import Game
move :: Kinoko -> Kinoko
move n@Kinoko {offset = offset, velocity = velocity} 
	= n {offset = offset''} where
		offset' = offset + velocity
		offset'' = if offset' < 0 || offset' + kinokoWidth > screenWidth 
					then offset
					else offset'

updateKinoko :: Kinoko -> Kinoko
updateKinoko n@Kinoko {velocity = velocity, frame = frame, status = status}
		| velocity < 0 = n {frame = frame', status = MoveLeft}
		| velocity > 0 = n {frame = frame', status = MoveRight}
		| otherwise    = n {frame = 0, status = status}
	where
		frame' = if (frame + 1) >= kinokoWalkFrames 
			then 0
			else frame + 1

showKinoko Kinoko { offset = x, frame = f, status = MoveLeft } src dst clipsLeft _   = applySurface x (screenHeight - kinokoHeight) src dst (Just (clipsLeft ! (fromIntegral f)))
showKinoko Kinoko { offset = x, frame = f, status = MoveRight } src dst _ clipsRight = applySurface x (screenHeight - kinokoHeight) src dst (Just (clipsRight ! (fromIntegral f)))

