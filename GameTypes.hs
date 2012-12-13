module GameTypes where

import Graphics.UI.SDL hiding (Event)
data Direction = LeftDir | RightDir deriving (Show, Eq)
data Label = Block | Player | Bonus | Signal deriving (Show, Eq)


data Trigger = Trigger {
	hitBlockT :: Event AEnv,
	hitBlockS :: Maybe AEnv,
	hitBlockTopT :: Event AEnv,
	hitBlockTopS :: Maybe AEnv,
  hitBonusT :: Event AEnv,
  hitBonusS :: Maybe AEnv,
  hitSignalT :: Event AEnv,
  hitSignalS :: Maybe AEnv,
  trgStepS :: Int,
  hitGroundT :: Event Double,
  hitGroundS :: Maybe Double,
	fallT :: Event (),
	fallS :: Maybe ()

}
data Actor = Actor { 
	position :: Behavior P2,
	positionS :: P2, 
	velocity :: Behavior P2, 
	velocityS :: P2, 
	accel :: P2,
	direction :: Direction, 
	image :: [String], 
	step :: Behavior Int, 
	stepS :: Int, 
  point :: Int,
	surface :: [Surface], 
	label :: Label, 
	width :: Double, 
	height :: Double, 
	triggers :: Trigger}


data P2 = P2 Double Double deriving (Show, Eq)
instance Num P2 where
   P2 x1 y1 + P2 x2 y2  = P2 (x1+x2) (y1+y2)
   P2 x1 y1 - P2 x2 y2  = P2 (x1-x2) (y1-y2)
   negate (P2 x y)      = P2 (-x) (-y)
   (*)                  = error "No * method for P2"
   abs                  = error "No abs method for P2"
   signum               = error "No * method for P2"
   fromInteger 0        = P2 0 0
   fromInteger _        = error "Only the constant 0 can be used as a P2"
instance Num a => Num (Behavior a) where
  (+)           = lift2 (+)
  (-)           = lift2 (-)
  (*)           = lift2 (*)
  abs           = lift1 abs
  negate        = lift1 negate
  fromInteger i = lift0 (fromInteger i)
  signum        = error "Cant use signum on behaviors"
  
-- Make methods in Fractional reactive

instance Fractional a => Fractional (Behavior a)
  where
    (/) = lift2 (/)
    fromRational r = lift0 (fromRational r)
 
type BD  = Behavior Double
type BB  = Behavior Bool
type BP2 = Behavior P2

type Stimulus = (Double, Maybe GameEvent, Env)
data Behavior a = Behavior (Stimulus -> IO (Behavior a, a))
data Event a = Event (Stimulus -> IO (Event a, Maybe a))
type ID = Int
type AEnv = [(ID, Actor)]
type Env = (AEnv, Int)


data GameEvent = EvHitblock | EvStep Int | EvHitground | EvFall | EvHitbonus | EvHitsignal | EvSignal SignalColor
		deriving (Eq, Show)

data SignalColor = Red | Yellow | Green
    deriving (Eq, Show)

data PlayerState = PlayerState {
	stateName :: String,
	startPos :: Maybe P2,
	moveVelocity :: P2,
	moveAccel :: P2,
	transition :: [Transition]
} deriving (Show)

data Transition = Transition {
	onevent :: GameEvent,
	toStateName :: String
} deriving (Show)

type States = [PlayerState]


fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b
trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c		


p2 :: BD -> BD -> BP2
p2 = lift2 P2

lift0 :: a -> Behavior a
lift0 v = f
  where f = Behavior (\_ -> return (f, v))

lift1 :: (a -> b) -> Behavior a -> Behavior b
lift1 fn (Behavior a0) = f a0
  where f a = Behavior (\s -> do (Behavior a', av) <- a s
                                 return (f a', fn av))

lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
lift2 fn (Behavior a0) (Behavior b0) = f a0 b0
  where f a b = Behavior (\s -> do (Behavior a', av) <- a s
                                   (Behavior b', bv) <- b s 
                                   return (f a' b', fn av bv))

lift3 :: (a -> b -> c -> d) -> Behavior a -> Behavior b -> Behavior c -> Behavior d
lift3 fn (Behavior a0) (Behavior b0) (Behavior c0) = f a0 b0 c0
  where f a b c = Behavior (\s -> do (Behavior a', av) <- a s
                                     (Behavior b', bv) <- b s
                                     (Behavior c', cv) <- c s
                                     return (f a' b' c', fn av bv cv))




(>*), (<*), (>=*), (<=*), (==*) :: BD -> BD -> BB
(>*)  = lift2 (>)
(<*)  = lift2 (<)
(>=*) = lift2 (>=)
(<=*) = lift2 (<=)
(==*) = lift2 (==)

-- Reactive and / or

(&&*), (||*) :: BB -> BB -> BB
(&&*) = lift2 (&&)
(||*) = lift2 (||)

-- This class represents values that can be scaled

class Num a => Vec a where
  (*^) :: Double -> a -> a

instance Vec Double where
  (*^) = (*)

instance Vec P2 where
  d *^ (P2 x y) = P2 (d*x) (d*y)


integral :: Vec a => Behavior a -> Behavior a
integral (Behavior a0) = Behavior (\(t, _, _) -> return (f t 0 a0, 0)) where
                  f tick acc a = Behavior (\s@(t, _, _) -> do 
                  							(Behavior a', av) <- a s 
                  							return (f t (acc + (t - tick) *^ av) a', acc))



