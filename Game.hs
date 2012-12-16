module Game where
import Control.Monad.State
import Control.Monad
import Data.List
import Data.Maybe
import Foreign hiding (unsafePerformIO)
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

import Graphics.UI.SDL hiding (Event)
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.Rotozoomer
import Graphics.UI.SDL.TTF
import Graphics.UI.SDL.Utilities as Util
import Data.Word (Word32, Word8)

import Const
import GameTypes
import Events
import Graphic
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

setPositionSVelocityS :: ID -> P2 -> P2 -> StateT Env IO ()
setPositionSVelocityS ident p v = do
	(actors, cnt) <- get
	let (actor, otherActors) = partition (\x -> fst x == ident) actors
	unless (length actor /= 1) $
		do 
			let a = head actor
			let newActor = (fst a, (snd a) {positionS = p, velocityS = v})
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
	setPositionSVelocityS ident pos0 vel0

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

getBonusNumber :: StateT Env IO Int
getBonusNumber = fmap length $ idsWithLabel Bonus


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
				liftIO $ putStrLn $ "ground :::::" ++ (show $ fromJust $ hitGroundS trg)
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

foreign import ccall unsafe "SDL_GetKeyState" sdlGetKeyState :: Ptr CInt -> IO (Ptr Word8)

type KeyProc = SDLKey -> Bool
-- this function comes from mokehehe's super nario bros: http://github.com/mokehehe/monao in the file "AppUtil.hs"
getKeyState :: IO KeyProc
getKeyState = alloca $ \numkeysPtr -> do
    keysPtr <- sdlGetKeyState numkeysPtr
    return $ \k -> (/= 0) $ unsafePerformIO $ (peekByteOff keysPtr $ fromIntegral $ Util.fromEnum k :: IO Word8)

initAnime :: StateT Env IO () -> StateT Env IO (Surface, Font, Word32, Int)
initAnime initEnv = do
	liftIO TTFG.init
	screen <- liftIO $ getScreen
	font <- liftIO $ openFont fontName fontSize
	initEnv
	t0 <- liftIO getTicks
	bonusNumber <- getBonusNumber
	return (screen, font, t0, bonusNumber)

animate :: States -> StateT Env IO () -> StateT Env IO ()
animate states initEnv = do

	(screen, font, t0, bonusNumber) <- initAnime initEnv

	let runit curState = do
		t <- liftIO getTicks
		sdlEvent <- liftIO pollEvent
		case sdlEvent of
			_ -> return ()

		let ft = fromIntegral (fromIntegral (t - t0)) / 1000
		trg <- run (ft, Nothing, ([], 0)) -- what??
		let playerEvents = getPlayerEventsFromTriggers trg
		--liftIO $ print playerEvents
		player <- getPlayerActor
		draw screen font player bonusNumber
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
		--liftIO $ putStrLn $ "TIME::::::::::: " ++ show (t2 - t)
		runit next

	let startState = getPlayerStateByName "start" states
	ident <- getPlayerId
	setMaybeMotion ident (startPos startState) (moveVelocity startState) (moveAccel startState)
	runit $ startState 

getKeyboardEvent :: IO [GameEvent]
getKeyboardEvent = do
	keyState <- liftIO getKeyState
	let keyLeft = keyState SDLK_LEFT
	let keyRight = keyState SDLK_RIGHT
	let keyUp = keyState SDLK_UP	
	if keyLeft && not keyUp
		then return [EvKeyLeft]
		else if keyRight && not keyUp
			then return [EvKeyRight]
			else if keyUp && not keyLeft && not keyRight
				then return [EvKeyUp]
				else if keyUp && keyLeft && not keyRight
					then return [EvKeyUpLeft]
					else if keyUp && keyRight && not keyLeft
						then return [EvKeyUpRight]
						else return []


animateKeyboard :: StateT Env IO () -> StateT Env IO ()
animateKeyboard initEnv = do

	(screen, font, t0, bonusNumber) <- initAnime initEnv

	let runit curState = do
		t <- liftIO getTicks
		quit <- do
			sdlEvent <- liftIO pollEvent
			if sdlEvent == Quit 
				then return True
				else return False
		when (not quit) $ do	
			let ft = fromIntegral (fromIntegral (t - t0)) / 1000
			trg <- run (ft, Nothing, ([], 0)) -- what??
			keyEvents <- liftIO getKeyboardEvent
			let playerEvents = getPlayerEventsFromTriggers trg ++ 
				if stateName curState == "start" 
					then keyEvents
					else []

			player <- getPlayerActor
			draw screen font player bonusNumber
			let nextState = getNextState curState playerEvents keyBoardStates
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
			--liftIO $ putStrLn $ "TIME::::::::::: " ++ show (t2 - t)
			runit next

	let startState = getPlayerStateByName "start" keyBoardStates
	ident <- getPlayerId
	setMaybeMotion ident (startPos startState) (moveVelocity startState) (moveAccel startState)
	runit $ startState 
	liftIO $ putStrLn "quit"
	liftIO TTFG.quit


getActorNumber :: State Env () -> Env -> Int
getActorNumber proc st = length $ fst $ snd $ runState proc st


setMaybeMotion :: ID -> Maybe P2 -> P2 -> P2 -> StateT Env IO ()
setMaybeMotion ident mpos0 vel0 accel = do
	actor <- getActorById ident
	unless (isNothing actor) $ do
		if isNothing mpos0
			then setMotion ident (positionS $ fromJust actor) vel0 accel
			else setMotionUser ident (fromJust mpos0) vel0 accel

getPlayerStateByName :: String -> States -> PlayerState
getPlayerStateByName name states = case (length xs) of
							1 -> head xs
							0 -> error $ "State " ++ (show name) ++ " not defined"
							_ -> error $ "State " ++ (show name) ++ " duplicated"	
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
	
newNikki :: Double -> Double -> StateT Env IO ID
newNikki x y = do
	ident <- newActor ["img/walk_right_00.png", "img/walk_right_01.png", "img/walk_right_02.png", "img/walk_right_03.png", "img/jump_right_00.png"] Player 78 104
	setTriggersForPlayer
	setMotionUser ident (P2 x y) (P2 0 0) (P2 0 0)
	return ident

newBox :: Double -> Double -> StateT Env IO ID
newBox x y = do
	ident <- newActor ["img/box.png"] Block 66 66
	setMotionUser ident (P2 x y) (P2 0 0) (P2 0 0)
	return ident

newStar :: Double -> Double -> StateT Env IO ID
newStar x y = do
	ident <- newActor ["img/star.png"] Bonus 62 66
	setMotionUser ident (P2 x y) (P2 0 0) (P2 0 0)
	return ident 

newSignal :: Double -> Double -> StateT Env IO ID
newSignal x y = do
	ident <- newActor ["img/red.png", "img/yellow.png", "img/green.png"] Signal 43 200
	setMotionUser ident (P2 x y) (P2 0 0) (P2 0 0)
	return ident

newBorders :: StateT Env IO (ID, ID)
newBorders = do
	leftBorder <- newActor [] Block 20 600
	setMotionUser leftBorder (P2 (-20) 0) (P2 0 0) (P2 0 0)

	rightBorder <- newActor [] Block 20 600
	setMotionUser rightBorder (P2 (fromIntegral sceneWidth) 0) (P2 0 0) (P2 0 0)

	return (leftBorder, rightBorder)



setting0 :: StateT Env IO ()
setting0 = do
	newNikki 100 0
	newBox 0 0
	newBorders
	return ()

setting1 :: StateT Env IO ()
setting1 = do 
    newNikki 20 0
    newBox 500 0
    newBorders
    return ()

setting2 :: StateT Env IO ()
setting2 = do
	newNikki 200 0
	newBorders
	newBox 100 0
	newBox 100 66
	newBox 500 0
	newBox 500 66
	newStar 110 132
	newStar 510 132
	return ()

setting3 :: StateT Env IO ()
setting3 = do
    newNikki 20 0

    newBox 200 0
    newStar 210 66
    newBox 270 0
    newStar 280 66
    newBox 340 100
    newStar 350 166
    newBox 410 100
    newStar 420 166
    newBox 480 200
    newStar 490 266
    newBox 550 200
    newStar 560 266
    return ()

setting4 :: StateT Env IO ()
setting4 = do
	newNikki 200 0
	newBorders
	newBox 300 0
	newBox 300 66
	newBox 300 132
	newBox 300 198
	newStar 310 264
	return ()	

setting5 :: StateT Env IO ()
setting5 = do
	newNikki 50 0
	newBox 300 300
	newBox 600 300
	newBox 900 300
	newBox 1200 300
	newStar 200 0
	newStar 300 0
	newSignal 600 0
	newBox 800 0
	newBox 866 0
	newStar 810 66
	newStar 876 66
	newBorders
	return ()
setting6 :: StateT Env IO ()
setting6 = do
	newNikki 20 0

	newBox 150 120
	newBox 215 120

	newBox 150 380
	newBox 215 380

	newBox 450 250
	newBox 515 250


	newStar 215 188
	newStar 215 446
	newStar 515 316

	newSignal 650 0

	newBox 800 0
	newBox 800 66
	newBox 800 132

	newStar 800 198

	newBox 1000 132
	newBox 1000 198
	newBox 1000 264

	newStar 1000 330

	newBox 1200 0
	newBox 1200 66
	newBox 1200 132

	newStar 1200 198

	newBorders
	
	return ()



setting11 :: StateT Env IO ()
setting11 = do 
	newNikki 20 0
	return ()

setting12 :: StateT Env IO ()
setting12 = do 
	newNikki 20 0
	newBorders
	return ()
keyBoardStates :: States
keyBoardStates = [
	PlayerState {
		stateName = "left",
		startPos = Nothing,
		moveVelocity = P2 (-100) 0,
		moveAccel = P2 0 0,
		transition = [
			Transition {onevent = EvStep 40,
						toStateName = "start"},
			Transition {onevent = EvHitground,
						toStateName = "start"},
			Transition {onevent = EvHitblock,
						toStateName = "start"}
		]
	},
	PlayerState {
		stateName = "right",
		startPos = Nothing,
		moveVelocity = P2 100 0,
		moveAccel = P2 0 0,
		transition = [
			Transition {onevent = EvStep 40,
						toStateName = "start"},
			Transition {onevent = EvHitground,
						toStateName = "start"},
			Transition {onevent = EvHitblock,
						toStateName = "start"}
		]	
	},
	PlayerState {
		stateName = "jumpUp",
		startPos = Nothing,
		moveVelocity = P2 0 (-150),
		moveAccel = P2 0 100,
		transition = [
			Transition {onevent = EvHitground,
						toStateName = "start"},
			Transition {onevent = EvHitblock,
						toStateName = "start"}
		]
	},
	PlayerState {
		stateName = "jumpLeft",
		startPos = Nothing,
		moveVelocity = P2 (-40) (-150),
		moveAccel = P2 0 100,
		transition = [
			Transition {onevent = EvHitground,
						toStateName = "start"},
			Transition {onevent = EvHitblock,
						toStateName = "start"}
		]
	},
	PlayerState {
		stateName = "jumpRight",
		startPos = Nothing,
		moveVelocity = P2 40 (-150),
		moveAccel = P2 0 100,
		transition = [
			Transition {onevent = EvHitground,
						toStateName = "start"},
			Transition {onevent = EvHitblock,
						toStateName = "start"}
		]
	},
	PlayerState {
		stateName = "start",
		startPos = Nothing,
		moveVelocity = P2 0 0,
		moveAccel = P2 0 100,
		transition = [
			Transition {onevent = EvKeyLeft,
						toStateName = "left"},
			Transition {onevent = EvKeyRight,
						toStateName = "right"},
			Transition {onevent = EvKeyUp,
						toStateName = "jumpUp"},
			Transition {onevent = EvKeyUpLeft,
						toStateName = "jumpLeft"},
			Transition {onevent = EvKeyUpRight,
						toStateName = "jumpRight"}
		]	
	} ]