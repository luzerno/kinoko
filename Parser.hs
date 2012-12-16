module Parser where

import Text.Parsec
import Lexer
import Ast
import Debug.Trace
import GameTypes
type Parser = Parsec [Token] ()

parsT = parse program "?" tok
    where Right tok = parse lexer "?" "start :\n jumpleftlow\n on hitground walk"


program :: Parser Program
program = many stmt

stmt :: Parser Statement
stmt = do a <- name
          b <- colon
          c <- pPara
          trans <- pTrans
          k <- endStmt
          let (Input pos velo acc) = c
          return $ Statement a pos velo acc trans


coordinate = do a <- oParen
                b <- int
                c <- comma
                d <- int
                e <- cParen
                return (b, d)

{-- parse event---}
pEvent :: Parser GameEvent
pEvent = try (pEvHit) <|> try (pEvStep) <|> try (pEvHitground) <|> try (pEvFall) <|> try (pEvHitbonus) <|> try (pEvHitsignal) <|> (pEvSignal)
pEvHitground :: Parser GameEvent
pEvHitground = do  pHitground
                   return EvHitground
pEvFall :: Parser GameEvent
pEvFall = do  pFall
              return EvFall
pEvHitsignal :: Parser GameEvent
pEvHitsignal = do pHitsignal
                  return EvHitsignal

pEvHitbonus :: Parser GameEvent
pEvHitbonus = do pHitbonus
                 return EvHitbonus

pEvSignal :: Parser GameEvent
pEvSignal = do pSignal
               equ
               c <- pInGreen <|> pInRed <|> pInYellow
               return $ EvSignal c

pInRed :: Parser SignalColor
pInRed = do pRed
            return Red

pInGreen :: Parser SignalColor
pInGreen = do pGreen
              return Green

pInYellow :: Parser SignalColor
pInYellow = do pYellow
               return Yellow

pEvHit :: Parser GameEvent
pEvHit = do pHitblock
            return EvHitblock

pEvStep :: Parser GameEvent
pEvStep = do pStep
             equ
             c <- int
             return $ EvStep c

pNextStatement :: Parser String
pNextStatement = name

pTransOne = do a <- pOn
               b <- pEvent
               c <- pNextStatement
               return $ Trans b c

pTrans = many pTransOne

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
pAuto = try (pInWalkRight) <|> try (pInWalkRightSlow) <|> try (pInWalkRightFast)
             <|>try (pInWalkLeft)  <|> try (pInWalkLeftSlow)  <|> try (pInWalkLeftFast)
             <|>try (pInJumpUp)    <|> try (pInJumpUpLow)     <|> try (pInJumpUpHigh)
             <|>try (pInJumpRight) <|> try (pInJumpRightLow)  <|> try (pInJumpRightHigh)
             <|>try (pInJumpLeft)  <|> try (pInJumpLeftLow)   <|> try (pInJumpLeftHigh)
             <|>try (pInStand)
 
pPara = try (pInput) <|> try (pAuto)

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

