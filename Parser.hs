module Parser where

import Text.Parsec
import Lexer
import Ast
import Debug.Trace
import GameTypes
type Parser = Parsec [Token] ()

parsT = parse program "?" tok
    where Right tok = parse lexer "?" "start :\n jumpleftlow\n on hitground walk"

{--
 "walk :\n positiond(9,9)\n velocity(8,8)\n acceleration(7,7)\n on hitblock walk"
 "jump :\n position(9,9)\n velocity(8,8)\n acceleration(7,7)\n on hitblock walk"
 "start :\n position(9,9)\n velocity(8,8)\n acceleration(7,7)\n on hitblock walk"
 "stop :\n on step = 9 walk"
--}

program :: Parser Program
program = do a <- (many stmt)
             return a

stmt :: Parser Statement
stmt = do a <- try (walkStmt) <|> try (startStmt) <|> try (jumpStmt) <|> try (stopStmt)
          return a


coordinate = do a <- oParen
                b <- int
                c <- comma
                d <- int
                e <- cParen
                return (b, d)

{-- parse event---}
pEvent :: Parser GameEvent
pEvent = do a <- try (pEvHit) <|> try (pEvStep) <|> try (pEvHitground) <|> try (pEvFall) <|> try (pEvHitbonus) <|> try (pEvHitsignal) <|> (pEvSignal)
            return a
pEvHitground :: Parser GameEvent
pEvHitground = do  a <- pHitground
                   return EvHitground
pEvFall :: Parser GameEvent
pEvFall = do  a <- pFall
              return EvFall
pEvHitsignal :: Parser GameEvent
pEvHitsignal = do a <- pHitsignal
                  return EvHitsignal

pEvHitbonus :: Parser GameEvent
pEvHitbonus = do a <- pHitbonus
                 return EvHitbonus

pEvSignal :: Parser GameEvent
pEvSignal = do a <- pSignal
               b <- equ
               c <- pInGreen <|> pInRed <|> pInYellow
               return $ EvSignal c

pInRed :: Parser SignalColor
pInRed = do a <- pRed
            return Red

pInGreen :: Parser SignalColor
pInGreen = do a <- pGreen
              return Green

pInYellow :: Parser SignalColor
pInYellow = do a <- pYellow
               return Yellow

pEvHit :: Parser GameEvent
pEvHit = do a <- pHitblock
            return EvHitblock

pEvStep :: Parser GameEvent
pEvStep = do a <- pStep
             b <- equ
             c <- int
             return $ EvStep c
{----parse event end---}


{--
pNextStatement :: Parser Token
pNextStatement = do a <- try (pWalk) <|> try (pJump) <|> try (pStop) 
                    return a
--}

pNextStatement :: Parser String
pNextStatement = do a <- name
                    return a

pTransOne = do a <- pOn
               b <- pEvent
               c <- pNextStatement
               return $ Trans b c

pTrans = do a <- many pTransOne
            return a
{-
pWithPos = do a <- pPosition
              pos <- coordinate
              d <- pVelo
              velo <- coordinate
              e <- pAcc
              acc <- coordinate
              trans <- pTrans
              k <- endStmt
              return $ pos velo acc trans

pWithoutPos = do a <- pVelo
                 velo <- coordinate
                 e <- pAcc
                 acc <- coordinate
                 trans <- pTrans
                 k <- endStmt
                 return $ (-1,-1) velo acc trans
-}
pPos = do a <- pPosition
          b <- coordinate
          return b

pInput :: Parser Input
pInput = do  pos <- option (-1,-1) pPos
             d <- pVelo
             velo <- coordinate
             e <- pAcc
             acc <- coordinate
             return $ Input pos velo acc

pAuto :: Parser Input
pAuto = do a <- try (pInWalkRight) <|> try (pInWalkRightSlow) <|> try (pInWalkRightFast)
             <|>try (pInWalkLeft)  <|> try (pInWalkLeftSlow)  <|> try (pInWalkLeftFast)
             <|>try (pInJumpUp)    <|> try (pInJumpUpLow)     <|> try (pInJumpUpHigh)
             <|>try (pInJumpRight) <|> try (pInJumpRightLow)  <|> try (pInJumpRightHigh)
             <|>try (pInJumpLeft)  <|> try (pInJumpLeftLow)   <|> try (pInJumpLeftHigh)
             <|>try (pInStand)
           return a
 
pPara = do a <- try (pInput) <|> try (pAuto)
           return a

pInWalkRight = do b <- option (-1,-1) pPos
                  a <- pWalkRight
                  return $ Input b (100,0) (0,0)

pInWalkRightSlow = do b <- option (-1,-1) pPos
                      a <- pWalkRightSlow
                      return $ Input b (50,0) (0,0)

pInWalkRightFast = do b <- option (-1,-1) pPos
                      a <- pWalkRightFast
                      return $ Input b (150,0) (0,0)

pInWalkLeft = do b <- option (-1,-1) pPos
                 a <- pWalkLeft
                 return $ Input b (-100,0) (0,0)

pInWalkLeftSlow = do b <- option (-1,-1) pPos
                     a <- pWalkLeftSlow
                     return $ Input b (-50,0) (0,0)

pInWalkLeftFast = do b <- option (-1,-1) pPos
                     a <- pWalkLeftFast
                     return $ Input b (-150,0) (0,0)

pInJumpUp = do b <- option (-1,-1) pPos
               a <- pJumpUp
               return $ Input b (0,-150) (0,100)

pInJumpUpLow = do b <- option (-1,-1) pPos
                  a <- pJumpUpLow
                  return $ Input b (0,-100) (0,100)

pInJumpUpHigh = do b <- option (-1,-1) pPos
                   a <- pJumpUpHigh
                   return $ Input b (0,-200) (0,100)

pInJumpRight = do b <- option (-1,-1) pPos
                  a <- pJumpRight
                  return $ Input b (40,-150) (0,100)

pInJumpRightLow = do b <- option (-1,-1) pPos
                     a <- pJumpRightLow
                     return $ Input b (40, -100) (0,100)

pInJumpRightHigh = do b <- option (-1,-1) pPos
                      a <- pJumpRightHigh 
                      return $ Input b (40,-200) (0,100)

pInJumpLeft = do b <- option (-1,-1) pPos
                 a <- pJumpLeft
                 return $ Input b (-40, -150) (0,100)

pInJumpLeftLow = do b <- option (-1,-1) pPos
                    a <- pJumpLeftLow
                    return $ Input b (-40,-100) (0,100)

pInJumpLeftHigh = do b <- option (-1,-1) pPos
                     a <- pJumpLeftHigh
                     return $ Input b (-40,-200) (0,100) 

pInStand = do b <- option (-1,-1) pPos
              a <- pStand
              return $ Input b (0,0) (0,0)

{--
walkStmt :: Parser Statement
walkStmt = do a <- pWalk
              b <- colon
              c <- try (pWithPos) <|> try (pWithoutPos)
              return $ Walk c

walkStmt = do a <- name
              b <- colon
              pos <- option (-1,-1) pPos
              d <- pVelo
              velo <- coordinate
              e <- pAcc
              acc <- coordinate
              trans <- pTrans
              k <- endStmt
              return $ Statement a pos velo acc trans
--}


walkStmt :: Parser Statement
walkStmt = do a <- name
              b <- colon
              c <- pPara
              trans <- pTrans
              k <- endStmt
              let (Input pos velo acc) = c
              return $ Statement a pos velo acc trans


startStmt :: Parser Statement
startStmt = do a <- name
               b <- colon
               c <- pPara
               trans <- pTrans
               k <- endStmt
               let (Input pos velo acc) = c
               return $ Statement a pos velo acc trans
jumpStmt :: Parser Statement
jumpStmt = do a <- name
              b <- colon
              c <- pPara
              trans <- pTrans
              k <- endStmt
              let (Input pos velo acc) = c
              return $ Statement a pos velo acc trans
{--
walkStmt :: Parser Statement
walkStmt = do a <- name
              b <- colon
              c <- pPara
              trans <- pTrans
              k <- endStmt
              let Input parameter = c
              return $ Statement a parameter trans


startStmt = do a <- name
               b <- colon
               c <- pPosition
               pos <- coordinate
               d <- pVelo
               velo <- coordinate
               e <- pAcc
               acc <- coordinate
               trans <- pTrans
               k <- endStmt
               return $ Statement a pos velo acc trans


jumpStmt =  do a <- name
               b <- colon
               pos <- option (-1,-1) pPos
               d <- pVelo
               velo <- coordinate
               e <- pAcc
               acc <- coordinate
               trans <- pTrans
               k <- endStmt
               return $ Statement a pos velo acc trans
--}

stopStmt = do a <- name
              b <- colon
              trans <- pTrans
              k <- endStmt
              return $ Statement a (-1,-1) (-1,-1) (-1,-1) trans