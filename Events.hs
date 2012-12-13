module Events where
import GameTypes
import Const
import Data.List

hitP2 :: (P2, (Double, Double)) -> (P2, (Double, Double)) -> Bool
hitP2 (P2 x1 y1, (w1, h1)) (P2 x2 y2, (w2, h2)) =  
	not $ x1 + w1 <= x2 || x1 >= x2 + w2 || y1 + h1 <= y2 || y1 >= y2 + h2

hitTopP2 :: (P2, (Double, Double)) -> (P2, (Double, Double)) -> Bool
hitTopP2 (P2 x1 y1, (w1, h1)) (P2 x2 y2, (w2, h2)) =
	let 
		xmin = min x1 x2
		xmax = max (x1 + w1) (x2 + w2)
		ymin = min y1 y2
		ymax = max (y1 + h1) (y2 + h2)
		xlap = w1 + w2 - (xmax - xmin)
		ylap = h1 + h2 - (ymax - ymin) in
	 xlap > 0 && ylap > 0 && xlap >= ylap

hitSide :: (P2, (Double, Double)) -> (P2, (Double, Double)) -> Bool
hitSide pt1 pt2 = hitP2 pt1 pt2 && not (hitTopP2 pt1 pt2)

hitTop :: (P2, (Double, Double)) -> (P2, (Double, Double)) -> Bool
hitTop pt1 pt2 = hitP2 pt1 pt2 && hitTopP2 pt1 pt2

mkHitEvent :: ID -> ((P2, (Double, Double)) -> (P2, (Double, Double)) -> Bool) -> Label -> Event AEnv
mkHitEvent ident hitFn lbl = e where
		e = Event (\(_, _, as) -> do
		let ([me], others) = partition (\x -> fst x == ident) $ fst as
		--let me = head as
		--let others = tail as
		let myPos = (positionS $ snd me, (width $ snd me, height $ snd me))
		let otherActors = filter (\x -> label x == lbl) $ map snd others

		let otherPos = zip (map positionS otherActors) $ zip (map width otherActors) (map height otherActors)
		let hitResBool = map (hitFn myPos) otherPos

		let hitAEnv = map snd $ filter fst $ zip hitResBool $ filter (\x -> (label $ snd x) == lbl) others
		if length hitAEnv >= 1 
			then return (e, Just hitAEnv)
			else return (e, Nothing))

hitBlockEvent :: ID -> Event AEnv
hitBlockEvent ident = mkHitEvent ident hitSide Block

hitTopEvent :: ID -> Event AEnv
hitTopEvent ident = mkHitEvent ident hitTop Block
	
hitBonusEvent :: ID -> Event AEnv
hitBonusEvent ident = mkHitEvent ident hitP2 Bonus

hitSignalEvent :: ID -> Event AEnv
hitSignalEvent ident = mkHitEvent ident hitP2 Signal

hitGroundEvent :: ID -> Event Double
hitGroundEvent ident = e where
	e = Event (\(_, _, as) -> do
		let ([me], others) = partition (\x -> fst x == ident) $ fst as
		let myPos = (positionS $ snd me, (width $ snd me, height $ snd me))
		let P2 _ velY = velocityS $ snd me
		let P2 _ posY = positionS $ snd me
		if velY <= 0
			then return (e, Nothing)
			else if posY + height (snd me) >= groundLevel
				then return (e, Just groundLevel)
				else do
					let otherActors = filter (\x -> label x == Block) $ map snd others
					let otherPos = zip (map positionS otherActors) $ zip (map width otherActors) (map height otherActors)
					let hitResBool = map (hitTop myPos) otherPos
					let hitPosY = map ((\(P2 x y) -> y) . snd) $ filter fst $ zip hitResBool $ map (positionS . snd) $ tail $ fst as
					if length hitPosY >= 1
						then return (e, Just $ minimum hitPosY)
						else return (e, Nothing)
		)

fallEvent :: ID -> Event ()
fallEvent ident = e where
	e = Event (\(_, _, as) -> do
		let ([me], others) = partition (\x -> fst x == ident) $ fst as
		let P2 posX posY = positionS $ snd me
		let myPos = (P2 posX (posY + 1), (width $ snd me, height $ snd me))
		let P2 _ accelY = accel $ snd me
		if (posY + height (snd me)) == groundLevel || accelY /= 0
			then return (e, Nothing)
			else do
			let otherActors = map snd others
			let otherPos = zip (map positionS otherActors) $ zip (map width otherActors) (map height otherActors)
			let hitResBool = map (hitP2 myPos) otherPos
			if True `elem` hitResBool 
				then return (e, Nothing)
				else return (e, Just ())
		)