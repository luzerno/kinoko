module Kinoko where
import Data.Array.IArray

import Util
import Const
import Game
move :: Kinoko -> Kinoko
move n@Kinoko {pos = pos, velocity = velocity} 
	= n {pos = pos''} where
		pos' = pos + velocity
		pos'' = if getX pos' < 0 || getX pos' + kinokoWidth > screenWidth 
					then pos
					else pos'

updateKinoko :: Kinoko -> Kinoko
updateKinoko n@Kinoko {velocity = velocity, frame = frame, status = status}
		| getX velocity < 0 = n {frame = frame', status = MoveLeft}
		| getX velocity > 0 = n {frame = frame', status = MoveRight}
		| otherwise    = n {frame = 0, status = status}
	where
		frame' = if (frame + 1) >= kinokoWalkFrames 
			then 0
			else frame + 1

showKinoko Kinoko { pos = P2 x y, frame = f, status = MoveLeft } src dst clipsLeft _   = applySurface x (screenHeight - kinokoHeight) src dst (Just (clipsLeft ! (fromIntegral f)))
showKinoko Kinoko { pos = P2 x y, frame = f, status = MoveRight } src dst _ clipsRight = applySurface x (screenHeight - kinokoHeight) src dst (Just (clipsRight ! (fromIntegral f)))

