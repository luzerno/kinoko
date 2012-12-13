module Ast where

import Lexer
import GameTypes
type Program = [Statement]

data Statement
  = Statement Id Position Velo Acc [Trans]
  deriving (Show, Eq)
{-
data Statement
  =   Walk  Position Velo Acc  [Trans]
   |  Start Position Velo Acc  [Trans]
   |  Jump  Position Velo Acc  [Trans]
   |  Stop  [Trans]
 deriving (Show, Eq)
-}
data Trans = Trans GameEvent NextStatement deriving (Show, Eq) 
type NextStatement = String
type Position = (Int, Int)
type Velo     = (Int, Int)
type Acc      = (Int, Int)
type Id = String
--data GameEvent = EvHitblock | EvStep Int | EvFall | EvHitground | EvHitbonus 
--                |EvHitsignal | EvSignal SignalColor
--                    deriving (Show, Eq)

data Input = Input Position Velo Acc
--data SignalColor = Red | Yellow |Green deriving (Show, Eq)