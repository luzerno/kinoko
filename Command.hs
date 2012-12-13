module Command where

import Lexer
import Parser
import Ast
import Text.Parsec
import GameTypes

--data Transition = Transition {
--    onevent :: GameEvent,
--    toStateName :: String
--} deriving (Show, Eq)

--data PlayerState = PlayerState {
--    stateName :: String,
--    startPos :: Maybe P2,
--    moveVelocity :: P2,
--    moveAccel :: P2,
--    transition :: [Transition]
--} deriving (Show , Eq)
 
--data P2 = P2 Double Double deriving (Show, Eq)

convertNextState :: Token -> String
convertNextState TokWalk = "walk"
convertNextState TokJump = "jump"
convertNextState TokStop = "stop"
convertNextState TokStart = "start"

test = convertS statement
    where Right statement = parse program "?" tok
          Right tok = parse lexer "?" "start : \n walkleft\n on hitblock walk2" 
{--
          Right tok = parse lexer "?" "stop1 :\n on step = 9 walk2\nwalk :\n velocity(8,8)\n acceleration(7,7)\n on hitblock walk3\n on step = 9 walk\njump :\n position(9,9)\n velocity(8,8)\n acceleration(7,7)\n on hitblock walk"
--}
convertS [] = []
convertS (x:xs) = convert x : convertS xs

convertTrans [] = []
convertTrans ((Trans event next):xs) = (Transition event next) : convertTrans xs

conCooMay :: (Int,Int) -> Maybe P2
conCooMay (a,b) = if a == -1 && b == -1 
                      then Nothing
                  else 
                      Just (P2 (fromIntegral a) (fromIntegral b))

conCoo :: (Int, Int) -> P2
conCoo (a,b) = if a == -1 && b == -1
                  then P2 0 0
               else
                  P2 (fromIntegral a) (fromIntegral b)

convert :: Statement -> PlayerState
convert (Statement id pos velo acc trans) = PlayerState {stateName = id, startPos = conCooMay pos,
                                               moveVelocity = conCoo velo, moveAccel = conCoo acc,
                                               transition = convertTrans trans}
{--
convert (Statement id pos velo acc trans) = PlayerState {stateName = id, startPos = conCooMay pos,
                                               moveVelocity = conCoo velo, moveAccel = conCoo acc,
                                               transition = convertTrans trans}
convert (Statement id pos velo acc trans) = PlayerState {stateName = id, startPos = conCooMay pos,
                                                  moveVelocity = conCoo velo, moveAccel = conCoo acc,
                                                  transition = convertTrans trans}

convert (Statement id pos velo acc trans) = PlayerState {stateName = id, startPos = Nothing,
                                    moveVelocity = P2 0 0, moveAccel = P2 0 0,
                                    transition = convertTrans trans}

--}