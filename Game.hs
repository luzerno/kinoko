module Game where
import Control.Monad.State
import Control.Monad
import Data.List
import Data.Maybe
import Data.IORef
import Graphics.UI.SDL hiding (Event)
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.Rotozoomer
import Graphics.UI.SDL.TTF
import Data.Word (Word32, Word8)

import Control.Monad.Reader
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Const
import GameTypes
import Events
import qualified Graphics.UI.SDL.TTF.General as TTFG


never :: Event a
never = Event (\s -> return (never, Nothing))
							
stepInc :: Behavior Int
stepInc = Behavior (\s -> return (f 1, 0)) where
			f x = Behavior (\s -> return (f $ x + 1, x))


newActor :: [String] -> Label -> Double -> Double -> StateT Env IO ID
newActor images label width height = do
	(actors, cnt) <- get
	liftIO $ print images
	liftIO $ print backgroundColor
	--let sur = error "surface not loaded"
	sur <- liftIO $ loadImages images
	let newTriggers = Trigger {
						hitBlockT = never, 
						hitBlockS = Nothing, 
						hitBlockTopT = never, 
						hitBlockTopS = Nothing, 
						hitBonusT = never,
						hitBonusS = Nothing,
						hitGroundT = never, 
						hitGroundS = Nothing,
						hitSignalT = never,
						hitSignalS = Nothing,
						fallT = never, 
						fallS = Nothing, 
						trgStepS = 0}
	let new = Actor { 
			position = error "position not inited", 
			positionS = error "positionS not inited", 
			velocity = error "velocity not inited", 
			velocityS = error "velocityS not inited", 
			accel = error "accel not inited", 
			image = images, 
			step = stepInc, 
			stepS = 0, 
			point = 0,
			surface = sur, 
			direction = RightDir, 
			label = label, 
			width = width, 
			height = height, 
			triggers = newTriggers }	

	put ((cnt + 1, new) : actors, cnt + 1)
	return $ cnt + 1

delActor :: ID -> StateT Env IO ()
delActor ident = do
	(actors, cnt) <- get
	let (actor, otherActors) = partition (\x -> fst x == ident) actors
	put (otherActors, cnt)

delActors :: [ID] -> StateT Env IO ()
delActors [] = return ()
delActors (x : xs) = do 
			delActor x
			delActors xs

setTriggers :: ID -> Trigger -> StateT Env IO ()
setTriggers ident t = do
	--liftIO $ putStrLn $ "set trigger for id " ++ show ident
	(actors, cnt) <- get
	let (actor, otherActors) = partition (\x -> fst x == ident) actors
	unless (length actor /= 1) $
		do 
			let a = head actor
			let newActor = (fst a, (snd a) {triggers = t})
			put (newActor : otherActors, cnt)


mkVelocityBehavior :: P2 -> P2-> Behavior P2
mkVelocityBehavior vel0 accel = lift0 vel0 + integral (lift0 accel)

mkPositionBehavior :: P2 -> Behavior P2 -> Behavior P2
mkPositionBehavior pos0 velB = lift0 pos0 + integral velB

mkMotion :: P2 -> P2 -> P2 -> Behavior P2
mkMotion pos0 vel0 accel = mkPositionBehavior pos0 $ mkVelocityBehavior vel0 accel

setVelocity :: ID -> Behavior P2 -> StateT Env IO ()
setVelocity ident b = do
	(actors, cnt) <- get
	let (actor, otherActors) = partition (\x -> fst x == ident) actors
	unless (length actor /= 1) $
		do
			let a = head actor
			let newActor = (fst a, (snd a) {velocity = b})			
			put (newActor : otherActors, cnt)


setPosition :: ID -> Behavior P2 -> StateT Env IO ()
setPosition ident b = do
	(actors, cnt) <- get
	let (actor, otherActors) = partition (\x -> fst x == ident) actors
	unless (length actor /= 1) $
		do 
			let a = head actor
			let newActor = (fst a, (snd a) {position = b})
			put (newActor : otherActors, cnt)

setAccel :: ID -> P2 -> StateT Env IO ()
setAccel ident accel = do
	(actors, cnt) <- get
	let (actor, otherActors) = partition (\x -> fst x == ident) actors
	unless (length actor /= 1) $
		do 
			let a = head actor
			let newActor = (fst a, (snd a) {accel = accel})
			put (newActor : otherActors, cnt)


setMotion :: ID -> P2 -> P2 -> P2 -> StateT Env IO ()
setMotion ident pos0 vel0 accel = do

	--let velocityB = mkVelocityBehavior vel0 $ mkAccelBehavior accel
	--let positionB = mkPositionBehavior pos0 velocityB
	let velocityB = lift0 vel0 + integral (lift0 accel)
	let positionB = lift0 pos0 + integral velocityB
	setAccel ident accel
	setVelocity ident velocityB
	setPosition ident positionB

addPoint :: ID -> StateT Env IO ()
addPoint ident = do
	(actors, cnt) <- get
	let (actor, otherActors) = partition (\x -> fst x == ident) actors
	unless (length actor /= 1) $ do
		let a = head actor
		let p = point $ snd a
		let newActor = (fst a, (snd a) {point = p + 1})
		put (newActor : otherActors, cnt)


setMotionUser :: ID -> P2 -> P2 -> P2 -> StateT Env IO ()
setMotionUser ident (P2 px py) vel0 accel = do
	actor <- getActorById ident
	let h = height $ fromJust actor
	setMotion ident (P2 px (fromIntegral screenHeight - h - py)) vel0 accel

setMoveRight :: ID -> P2 -> StateT Env IO ()
setMoveRight ident pos0 = setMotion ident pos0 (P2 40 0) (P2 0 0)

setMoveLeft :: ID -> P2 -> StateT Env IO ()
setMoveLeft ident pos0 = setMotion ident pos0 (P2 (-40) 0) (P2 0 0)

updateActor :: (ID, Actor) -> StateT Env IO ()
updateActor (ident, a) = do
	(actors, cnt) <- get
	let (actor, otherActors) = partition (\x -> fst x == ident) actors
	unless (length actor /= 1) $ 
		put ((ident, a) : otherActors, cnt)

updateActors :: [(ID, Actor)] -> StateT Env IO ()
updateActors as = do
	(actors, cnt) <- get
	let changedIds = map fst as
	let (changedActors, otherActors) = partition (\x -> fst x `elem` changedIds) actors
	unless (length changedActors /= 1) $
		put (as ++ otherActors, cnt)



setDirection :: ID -> Direction -> StateT Env IO ()
setDirection ident d = do
	(actors, cnt) <- get
	let (actor, otherActors) = partition (\x -> fst x == ident) actors
	unless (length actor /= 1) $ do
		let a = head actor
		let newActor = (fst a, (snd a) {direction = d})
		put (newActor : otherActors, cnt)


resetStep :: ID -> StateT Env IO ()
resetStep ident = do
	(actors, cnt) <- get
	let (actor, otherActors) = partition (\x -> fst x == ident) actors
	unless (length actor /= 1) $ do
		let a = head actor
		let newActor = (fst a, (snd a) {step = stepInc})
		put (newActor : otherActors, cnt)


getMotion :: ID -> State Env (Maybe (Behavior P2))
getMotion ident = do
	(actors, cnt) <- get
	let actor = fmap snd $ find (\x -> fst x == ident) actors
	if isNothing actor 
		then return Nothing
		else return $ position `fmap` actor



getActorById :: ID -> StateT Env IO (Maybe Actor)
getActorById ident = do
	(actors, cnt) <- get
	let actor = fmap snd $ find (\x -> fst x == ident) actors
	if isNothing actor
		then return Nothing
		else return actor

actorsWithLabel :: Label -> StateT Env IO [Actor]
actorsWithLabel l = do
	(idactors, cnt) <- get
	let actors = map snd idactors
	let actorsLabel = filter (\x -> label x == l) actors
	return actorsLabel

idsWithLabel :: Label -> StateT Env IO [ID]
idsWithLabel l = do
	(idactors, cnt) <- get
	let ids = map fst $ filter (\x -> label (snd x) == l) idactors
	return ids




liftState :: (a -> StateT Env IO b) -> [a] -> StateT Env IO [b]
liftState fn [] = return []
liftState fn (a : as) = do
	b <- fn a
	bs <- liftState fn as
	return (b : bs)




getNewActorPosition :: Stimulus -> Actor -> StateT Env IO Actor
getNewActorPosition s a = do
	let Behavior m = position a
	let Behavior v = velocity a
	let Behavior st = step a
	let d = direction a
	(newM, pos) <- liftIO $ m s
	(newV, vel) <- liftIO $ v s
	(newS, st) <- liftIO $ st s
	let P2 x _ = vel
	let dir
		| x == 0 = direction a
		| x > 0 = RightDir
		| otherwise = LeftDir
	--lift $ print dir
	--lift $ putStrLn $ "velocity " ++ (show vel)
	return $ a {position = newM, positionS = pos, velocity = newV, velocityS = vel, step = newS, stepS = st, direction = dir}


getPlayerId :: StateT Env IO ID
getPlayerId = do
	players <- idsWithLabel Player
	let player = head players
	return player


getPlayerActor :: StateT Env IO Actor
getPlayerActor = do
	players <- actorsWithLabel Player
	let player = head players
	return player



updateActorsMovementGetNewEnv :: Stimulus -> StateT Env IO Env
updateActorsMovementGetNewEnv s = do
	(idActors, cnt) <- get
	newActors <- liftState (getNewActorPosition s) (map snd idActors)
	let newEnv = (zip (map fst idActors) newActors, cnt)
	--put newEnv
	return newEnv

updateTriggers :: Stimulus -> StateT Env IO Env
updateTriggers s = do
	--(idActors, cnt) <- get
	let (idActors, cnt) = trd3 s
	let update a = do
		let (me, others) = partition (\x -> fst x == fst a) idActors
		let aenv = me ++ others
		--let s' = (fst3 s, snd3 s, aenv)
		let tg = triggers $ snd a 
		let Event hitblockEv = hitBlockT tg
		let Event hitgroundEv = hitGroundT tg
		let Event fallEv = fallT tg
		let Event hitBonusEv = hitBonusT tg
		let Event hitSignalEv = hitSignalT tg
		let stp = stepS $ snd a

		(hitblockEv', hitblockv) <- liftIO $ hitblockEv s
		(hitgroundEv', groundv) <- liftIO $ hitgroundEv s
		(fallEv', fallv) <- liftIO $ fallEv s
		(hitBonusEv', hitBonusv) <- liftIO $ hitBonusEv s
		(hitSignalEv', hitSignalv) <- liftIO $ hitSignalEv s
		--liftIO $ putStrLn $ "signal :::" ++ (show $ isNothing hitSignalv)

		let tg' = tg {hitBlockT = hitblockEv', hitBlockS = hitblockv, hitGroundT = hitgroundEv', hitGroundS = groundv, hitBonusT = hitBonusEv', hitBonusS = hitBonusv, hitSignalT = hitSignalEv', hitSignalS = hitSignalv, fallT = fallEv', fallS = fallv, trgStepS = stp}
		return (fst a, (snd a) {triggers = tg'})
	newIdActors <- liftState update idActors 
	--put (newIdActors, cnt)
	return (newIdActors, cnt)
	--put (idActors, cnt)


handleEventsForActors :: Env -> StateT Env IO ()
handleEventsForActors env =
	handleActorEvent $ fst env

handleActorEvent :: [(Int, Actor)] -> StateT Env IO ()
handleActorEvent ((ident, actor) : xs) = do
	let trg = triggers actor
	if label actor == Player
		then do
			Just player <- getActorById ident
			unless (isNothing $ hitBlockS trg) $ do
				let myPos = positionS player
				let P2 velX velY = velocityS player
				let P2 accelX accelY = accel player
				setMotion ident myPos (P2 0 velY) (P2 0 accelY)
				return ()

			unless (isNothing $ hitGroundS trg) $ do
				let posY = (fromJust $ hitGroundS trg) - height actor
				let P2 posX _ = positionS player
				setMotion ident (P2 posX posY) (P2 0 0) (P2 0 0)
				return ()
			unless (isNothing $ fallS trg) $ do
				let myPos = positionS actor
				let P2 velX _ = velocityS actor
				let P2 accelX _ = accel actor
				setMotion ident myPos (velocityS actor) (P2 0 (200))
				return ()
			unless (isNothing $ hitBonusS trg) $ do
				let bonus = map fst $ fromJust $ hitBonusS trg
				liftIO $ print $ zip (map fst $ fromJust $ hitBonusS trg) (map (label . snd) $ fromJust $ hitBonusS trg)
				addPoint ident
				delActors bonus

		else return ()
	handleActorEvent xs

handleActorEvent [] = do
	return ()



getPlayerCurrentEvents :: Env -> StateT Env IO Trigger
getPlayerCurrentEvents env = do
	playerId <- getPlayerId
	let player = snd $ head $ filter (\x -> fst x == playerId) $ fst env
	return $ triggers player
		

run :: Stimulus -> StateT Env IO Trigger
run s = do
	--liftIO $ putStrLn "Phase 1"
	env <- updateActorsMovementGetNewEnv s
	--liftIO $ putStrLn "Phase 2"
	env' <- updateTriggers (fst3 s, snd3 s, env)
	--liftIO $ putStrLn "Phase 3"
	handleEventsForActors env'
	env'' <- updateActorsMovementGetNewEnv s
	put env''

	playerCurrentEvents <- getPlayerCurrentEvents env'
	--put env'
-- test
	(idActors, cnt) <- get
	players <- actorsWithLabel Player
	--liftIO $ print $ positionS $ players !! 0
	--liftIO $ print $ map (isNothing . hitBlockS . triggers . snd) idActors
	return playerCurrentEvents

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

drawPoint :: Surface -> Font -> StateT Env IO ()
drawPoint screen font = do
	player <- getPlayerActor
	liftIO $ putPointToScreen screen font $ point player

putPointToScreen :: Surface -> Font -> Int -> IO ()
putPointToScreen screen font p = do
	message <- renderTextSolid font ("Points: " ++ (show p)) (Color 255 255 255)
	applySurface pointPosX pointPosY message screen
	return ()


getPlayerScreenPosition :: P2 -> Double -> Double -> P2
getPlayerScreenPosition (P2 px py) w h
	| px <= playerScreenPosX = P2 px py
	| px >= fromIntegral sceneWidth - fromIntegral screenWidth + playerScreenPosX = P2 (fromIntegral screenWidth - fromIntegral sceneWidth + px) py
	| otherwise = P2 playerScreenPosX py

drawActors :: Surface -> StateT Env IO ()
drawActors screen = do
	player <- getPlayerActor
	(actors, cnt) <- get
	let pos = positionS player
	let newPos = getPlayerScreenPosition pos (width player) (height player)
	let d = newPos - pos
	liftIO $ putActorsToScreen screen (map snd actors) d

draw :: Surface -> Font -> StateT Env IO ()
draw screen font = do 
		  rect <- liftIO $ getClipRect screen
		  black <- liftIO $ mapRGB (surfaceGetPixelFormat screen) 0x00 0x00 0x00
		  white <- liftIO $ mapRGB (surfaceGetPixelFormat screen) 0xFF 0xFF 0xFF
		  liftIO $ fillRect screen (Just rect) black 
		  drawActors screen
		  drawPoint screen font
		  liftIO $ Graphics.UI.SDL.flip screen
		    --delay 1
		  return ()

getStepFromGameEvents :: [GameEvent] -> Int
getStepFromGameEvents evs = foldl (\acc x -> acc + case x of
													EvStep s -> s
													_ -> 0) 0 evs

eventPartof :: GameEvent -> [GameEvent] -> Bool
eventPartof ev evs = case ev of 
						EvStep x -> getStepFromGameEvents evs >= x
						_ -> ev `elem` evs

getNextState :: PlayerState -> [GameEvent] -> States -> Maybe PlayerState
getNextState currentState events states =
	let 
		trans = transition currentState
		maybeNext = find (\x -> onevent x `eventPartof` events) trans in
	if isNothing maybeNext 
		then Nothing 
		else Just $ getPlayerStateByName (toStateName $ fromJust maybeNext) states 



getPlayerEventsFromTriggers :: Trigger -> [GameEvent]
getPlayerEventsFromTriggers trg = checkHitBlock trg ++ checkStep trg ++ checkHitGround trg ++ checkHitBonus trg ++ checkHitSignal trg

checkHitBlock :: Trigger -> [GameEvent]
checkHitBlock trg = if isNothing $ hitBlockS trg
						then []
						else [EvHitblock]
checkHitGround :: Trigger -> [GameEvent]
checkHitGround trg = if isNothing $ hitGroundS trg
						then []
						else [EvHitground]

checkStep :: Trigger -> [GameEvent]
checkStep trg = [EvStep $ trgStepS trg]


getSignalColor :: Trigger -> SignalColor
getSignalColor trg = 
		let step = stepS $ snd $ head $ fromJust $ hitSignalS trg in
		case (step `div` signalStep `mod` signalColors) of
			0 -> Red
			1 -> Yellow
			2 -> Green
			_ -> Red

checkHitSignal :: Trigger -> [GameEvent]
checkHitSignal trg = if isNothing $ hitSignalS trg
						then []
						else [EvHitsignal, EvSignal $ getSignalColor trg]
checkFall :: Trigger -> [GameEvent]
checkFall trg = if isNothing $ fallS trg
						then []
						else [EvFall]

checkHitBonus :: Trigger -> [GameEvent]
checkHitBonus trg = if isNothing $ hitBonusS trg
						then []
						else [EvHitbonus]

animate :: States -> StateT Env IO () -> StateT Env IO ()
animate states initEnv = do
	liftIO TTFG.init
	screen <- liftIO $ getScreen
	font <- liftIO $ openFont fontName fontSize
	initEnv
	t0 <- liftIO getTicks

	let runit curState = do
		t <- liftIO getTicks
		let ft = fromIntegral (fromIntegral (t - t0)) / 1000
		trg <- run (ft, Nothing, ([], 0)) -- what??
		let playerEvents = getPlayerEventsFromTriggers trg
		liftIO $ print playerEvents
		draw screen font
		let nextState = getNextState curState playerEvents states
		next <- if isNothing nextState
			then do
				--liftIO $ putStrLn "stay state"
				return curState
			else do
				--liftIO $ putStrLn "new State"
				let next = fromJust nextState
				ident <- getPlayerId
				resetStep ident
				setMaybeMotion ident (startPos next) (moveVelocity next) (moveAccel next)
				return next
		t2 <- liftIO getTicks
		if (t2 - t) < msPerTick
			then lift $ delay (msPerTick - t2 + t)
			else return ()
		liftIO $ putStrLn $ "TIME::::::::::: " ++ show (t2 - t)
		runit next

	let startState = getPlayerStateByName "start" states
	ident <- getPlayerId
	setMaybeMotion ident (startPos startState) (moveVelocity startState) (moveAccel startState)
	runit $ startState 



getActorNumber :: State Env () -> Env -> Int
getActorNumber proc st = length $ fst $ snd $ runState proc st


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



setMaybeMotion :: ID -> Maybe P2 -> P2 -> P2 -> StateT Env IO ()
setMaybeMotion ident mpos0 vel0 accel = do
	actor <- getActorById ident
	unless (isNothing actor) $ do
		if isNothing mpos0
			then setMotion ident (positionS $ fromJust actor) vel0 accel
			else setMotionUser ident (fromJust mpos0) vel0 accel

getPlayerStateByName :: String -> States -> PlayerState
getPlayerStateByName name states = if length xs == 1 
								then head xs
								else error "State name duplicated or not defined"
				where xs = filter (\x -> stateName x == name) states

setTriggersForPlayer :: StateT Env IO ()
setTriggersForPlayer = do
	playerId <- getPlayerId
	setTriggers playerId Trigger {
		hitBlockT = hitBlockEvent playerId, 
		hitBlockS = Nothing, 
		hitBlockTopT = hitTopEvent playerId, 
		hitBlockTopS = Nothing, 
		hitBonusT = hitBonusEvent playerId,
		hitBonusS = Nothing,
		hitGroundT = hitGroundEvent playerId, 
		hitGroundS = Nothing, 
		hitSignalT = hitSignalEvent playerId,
		hitSignalS = Nothing,
		fallT = fallEvent playerId, 
		fallS = Nothing, 
		trgStepS = 0}
	
setting1 :: StateT Env IO ()
setting1 = do
	a1 <- newActor ["img/walk_right_00.png", "img/walk_right_01.png", "img/walk_right_02.png", "img/walk_right_03.png", "img/jump_right_00.png"] Player 78 104
	--setMotion a1 (P2 30 300) (P2 40 0) (P2 0 0)
	setTriggersForPlayer

	a2 <- newActor ["img/box.png"] Block 66 66
	setMotionUser a2 (P2 300 300) (P2 0 0) (P2 0 0)

	a3 <- newActor ["img/box.png"] Block 66 66
	setMotionUser a3 (P2 600 300) (P2 0 0) (P2 0 0)

	a4 <- newActor ["img/box.png"] Block 66 66
	setMotionUser a4 (P2 900 300) (P2 0 0) (P2 0 0)

	a5 <- newActor ["img/box.png"] Block 66 66
	setMotionUser a5 (P2 1200 300) (P2 0 0) (P2 0 0)

	a6 <- newActor ["img/star.png"] Bonus 62 66
	setMotionUser a6 (P2 200 0) (P2 0 0) (P2 0 0)
	a7 <- newActor ["img/star.png"] Bonus 62 66
	setMotionUser a7 (P2 300 0) (P2 0 0) (P2 0 0)

	a8 <- newActor ["img/red.png", "img/yellow.png", "img/green.png"] Signal 43 200
	setMotionUser a8 (P2 600 0) (P2 0 0) (P2 0 0)




	
	leftBorder <- newActor [] Block 20 600
	setMotionUser leftBorder (P2 (-20) 0) (P2 0 0) (P2 0 0)

	rightBorder <- newActor [] Block 20 600
	setMotionUser rightBorder (P2 (fromIntegral sceneWidth) 0) (P2 0 0) (P2 0 0)

states :: States
states = [PlayerState { stateName = "start", 
						startPos = Just $ P2 30 0, 
						moveVelocity = P2 140 0, 
						moveAccel = P2 (-80) 0, 
						transition = [
							Transition {onevent = EvStep 700, 
										toStateName = "stop"}]},
		  PlayerState { stateName = "stop",
		  				startPos = Nothing,
		  				moveVelocity = P2 0 0,
		  				moveAccel = P2 0 0,
		  				transition = [
		  					Transition {onevent = EvStep 0,
		  							    toStateName = "jump"}]},
		  PlayerState { stateName = "jump",
		  				startPos = Nothing,
		  				moveVelocity = P2 60 (-200),
		  				moveAccel = P2 0 100,
		  				transition = [
		  					Transition {onevent = EvHitground,
		  								toStateName = "walk"}]},
		  PlayerState { stateName = "walk",
						startPos = Nothing,
						moveVelocity = P2 40 0,
						moveAccel = P2 0 0,
						transition = []}


		  	]

states2 = [PlayerState { stateName = "start",
						 startPos = Just $ P2 30 0,
						 moveVelocity = P2 (-40) (-150),
						 moveAccel = P2 0 100,
						 transition = []}]

states3 = [PlayerState {stateName = "start",
						startPos = Just $ P2 0 0,
						moveVelocity = P2 0 0,
						moveAccel = P2 0 0,
						transition = [
							Transition {onevent = EvStep 0, toStateName = "go"}
						]},
		   PlayerState {stateName = "go",
						startPos = Nothing,
						moveVelocity = P2 100 0,
						moveAccel = P2 0 0,
						transition = [
							Transition {onevent = EvHitblock, toStateName = "back"}
						]},
		   PlayerState {stateName = "back",
		  				startPos = Nothing,
		  				moveVelocity = P2 (-100) 0,
		  				moveAccel = P2 0 0,
		  				transition = [
		  					Transition {onevent = EvHitblock, toStateName = "go"}
		  				]}]

states4 = [PlayerState { stateName = "start",
						 startPos = Just $ P2 30 0,
						 moveVelocity = P2 (40) (0),
						 moveAccel = P2 0 0,
						 transition = [
						 	Transition {onevent = EvHitsignal,
						 				toStateName = "stop"}
						 ]},
		  PlayerState { stateName = "stop",
						startPos = Nothing,
						moveVelocity = P2 0 0,
						moveAccel = P2 0 0,
						transition = [
							Transition {onevent = EvSignal Red,
										toStateName = "walk"}]},
		  PlayerState { stateName = "walk",
						startPos = Nothing,
						moveVelocity = P2 (100) 0,
						moveAccel = P2 0 0,
						transition = []}
			]