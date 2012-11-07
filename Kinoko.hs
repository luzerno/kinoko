module Kinoko where
import Data.Array.IArray

import Util
import Const
import Game
move :: Kinoko -> Kinoko
move n@Kinoko {pos = pos, velocity = velocity, stand = stand} 
	= n {pos = pos'', velocity = velocity', stand = stand'} where
		pos' = pos + velocity
		pos'' = if getX pos' < 0 || getX pos' + kinokoWidth > screenWidth {-|| getY pos' < 0 || getY pos' + kinokoHeight > screenHeight -}
					then pos
					else pos'
		stand' = if stand == True
					then True
					else if getY pos'' >= ground
						then True
						else False
		velocity' = if stand' == True
						then P2 (getX velocity) 0
						else velocity + jumpAccel


updateKinoko :: Kinoko -> Kinoko
updateKinoko n@Kinoko {velocity = velocity, frame = frame, status = status}
		| getX velocity < 0 = n {frame = frame', status = MoveLeft}
		| getX velocity > 0 = n {frame = frame', status = MoveRight}
		| otherwise    = n {frame = 0, status = status}
	where
		frame' = if (frame + 1) >= kinokoWalkFrames 
			then 0
			else frame + 1

showKinoko Kinoko { pos = P2 x y, frame = f, status = MoveLeft } src dst clipsLeft _   = applySurface x  y src dst (Just (clipsLeft ! (fromIntegral f)))
showKinoko Kinoko { pos = P2 x y, frame = f, status = MoveRight } src dst _ clipsRight = applySurface x  y src dst (Just (clipsRight ! (fromIntegral f)))

