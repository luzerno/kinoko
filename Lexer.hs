module Lexer where

import Text.Parsec
import Text.Parsec.Pos (newPos)
import Text.Parsec.String
import Data.Char
import Debug.Trace

lexerT = parse lexer "?" "walk\n\nwalk\n"

type Name = String
type Indent = Int

data Token
     =   TokWalkRight
       | TokWalkRightSlow
       | TokWalkRightFast
       | TokWalkLeft
       | TokWalkLeftSlow
       | TokWalkLeftFast
       | TokJumpUp
       | TokJumpUpLow
       | TokJumpUpHigh
       | TokJumpRight
       | TokJumpRightLow
       | TokJumpRightHigh
       | TokJumpLeft
       | TokJumpLeftLow
       | TokJumpLeftHigh
       | TokJump
       | TokWalk  
       | TokStop
       | TokStart
       | TokStand
       | TokFall
       | TokHitground
       | TokHitbonus
       | TokHitsignal
       | TokSignal
       | TokInt Int
       | TokVar String
       | TokPosition
       | TokVelo 
       | TokAcc 
       | TokHitblock 
       | TokStep
       | TokOn 
       | TokRed
       | TokGreen
       | TokYellow
       | TokIndent Int
       | TokPad
       | TokEndStatement
       | TokEqu
       | TokPlus
       | TokMinus
       | TokColon
       | TokSpace
       | TokEndLine
       | TokComma
       | TokOParen
       | TokCParen
       | TokEmptyLine
    deriving (Show, Eq)

lexChar :: Char -> Token -> Parser Token
lexChar c t = do char c
                 return t

-- Parses a sequence of digits into a positive integer.
-- Does not handle negative numbers
lexPosInt :: Parser Int
lexPosInt = do a <- many1 digit
               let num = stringToInt a
               return num

lexInt :: Parser Int
lexInt = do a <- option TokPlus (lexChar '-' TokMinus)
            b <- lexPosInt
            if a /= TokMinus 
               then return b
            else return $ negate b

stringToInt :: [Char] -> Int
stringToInt xx = foldl (\x y -> 10 * x + (digitToInt y)) 0 xx

-- Parses a sequence of characters into an identifier.
-- A valid identifier starts with a letter or an underscore and contains
-- only digits, letters, and underscores
lexName :: Parser Name
lexName = do a <- letter <|> ( char '_') 
             b <- many (letter <|> digit <|> (char '_'))
             return $ a:b

endOfFile :: Parsec [Char] () Token
endOfFile = do eof
               return TokEndLine
{--
lexToken :: Parsec [Char] () Token
lexToken = do
    a <- try (string "walk")
    return TokWalk
    <|> do a <- try (string "jump")
           return TokJump
    <|> do a <- try (string "start")
           return TokStart
    <|> do a <- try (string "stop") 
           return TokStop
    <|> do a <- try (string "position")
           return TokPosition
    <|> do a <- try (string "velocity")
           return TokVelo
    <|> do a <- try (string "acceleration")
           return TokAcc
    <|> do a <- try (string "hitblock")
           return TokHitblock
    <|> do a <- try (string "step")
           return TokStep
    <|> do a <- try (string "on")
           return TokOn
    <|> do a <- try (string "fall")
           return TokFall
    <|> do a <- try (string "hitground")
           return TokHitground
    <|> do a <- lexInt
           return $ TokInt a
    <|> do a <- try lexName
           return $ TokVar a 
    <|> do lexChar '=' TokEqu
    <|> do lexChar ':' TokColon
    <|> do lexChar ' ' TokSpace
    <|> do lexChar ',' TokComma
    <|> do lexChar '(' TokOParen
    <|> do lexChar ')' TokCParen
   --}

lexToken :: Parsec [Char] () Token
lexToken = do 
    a <- try (string "position")
    return TokPosition
    <|> do a <- try (string "velocity")
           return TokVelo
    <|> do a <- try (string "acceleration")
           return TokAcc
    <|> do a <- try (string "hitblock")
           return TokHitblock
    <|> do a <- try (string "hitbonus")
           return TokHitbonus
    <|> do a <- try (string "hitsignal")
           return TokHitsignal
    <|> do a <- try (string "signal")
           return TokSignal
    <|> do a <- try (string "red")
           return TokRed
    <|> do a <- try (string "green")
           return TokGreen
    <|> do a <- try (string "yellow")
           return TokYellow
    <|> do a <- try (string "step")
           return TokStep
    <|> do a <- try (string "on")
           return TokOn
    <|> do a <- try (string "fall")
           return TokFall
    <|> do a <- try (string "hitground")
           return TokHitground
    <|> do a <- try (string "walkrightslow")
           return TokWalkRightSlow
    <|> do a <- try (string "walkrightfast")
           return TokWalkRightFast
    <|> do a <- try (string "walkright")
           return TokWalkRight
    <|> do a <- try (string "walkleftslow")
           return TokWalkLeftSlow
    <|> do a <- try (string "walkleftfast")
           return TokWalkLeftFast
    <|> do a <- try (string "walkleft")
           return TokWalkLeft
    <|> do a <- try (string "jumpuplow")
           return TokJumpUpLow
    <|> do a <- try (string "jumpuphigh")
           return TokJumpUpHigh
    <|> do a <- try (string "jumpup")
           return TokJumpUp
    <|> do a <- try (string "jumprightlow")
           return TokJumpRightLow
    <|> do a <- try (string "jumprighthigh")
           return TokJumpRightHigh
    <|> do a <- try (string "jumpright")
           return TokJumpRight
    <|> do a <- try (string "jumpleftlow")
           return TokJumpLeftLow
    <|> do a <- try (string "jumplefthigh")
           return TokJumpLeftHigh
    <|> do a <- try (string "jumpleft")
           return TokJumpLeft
    <|> do a <- try (string "stand")
           return TokStand
    <|> do a <- lexInt
           return $ TokInt a
    <|> do a <- try lexName
           return $ TokVar a 
    <|> do lexChar '=' TokEqu
    <|> do lexChar ':' TokColon
    <|> do lexChar ' ' TokSpace
    <|> do lexChar ',' TokComma
    <|> do lexChar '(' TokOParen
    <|> do lexChar ')' TokCParen


lexIndent :: Parser Indent
lexIndent = do a <- many (lexChar ' ' TokSpace)
               return $ length a


lexLineWithIndent :: Parser [Token]
lexLineWithIndent = do a <- try (lexEmptyLine) <|> try (lexLineWithIndent')
                       return a

lexLineWithIndent' :: Parser [Token]
lexLineWithIndent' = do a <- lexIndent 
                        b <- many1 lexToken
                        c <- try (lexChar '\n' TokEndLine) <|> try (endOfFile)
                        return $ (TokIndent a) : b ++ [c]

lexEmptyLine :: Parser [Token]
lexEmptyLine =  do a <- lexIndent
                   b <- (lexChar '\n' TokEndLine) 
                   return [TokSpace]


filterSpace tok = filter (/= TokSpace) tok
{-
filterEmptyLine tok = if (tok !! (i-1)) == TokIndent  

         where i = TokEndLine `elemIndex` tok 
-}
preprocess :: Parser [Token]
preprocess = do a <- many1 lexLineWithIndent
                let b = filterSpace (concat a)
                return b

filterTokPad tok = filter (/= TokPad) tok

indentationProcess [] = []
indentationProcess tok = statement ++ indentationProcess proTok
      where statement = getStatement tok
            proTok = drop len tok
            len = length statement 

lexer :: Parsec [Char] () [Token]
lexer = do tok <- preprocess
           let filteredTok = (filterTokPad (indentationProcess tok))
           return filteredTok

getStatement [] = []
getStatement tok 
  | (isStartStatement tok) = getStartStatement tok
  | (isWalkStatement tok) = getWalkStatement tok
  | (isJumpStatement tok) = getJumpStatement tok
  | (isStopStatement tok) = getStopStatement tok
isWalkStatement tok = if (tok !! 1) == TokWalk
                          then True
                      else 
                          True

isStartStatement tok = if (tok !! 1) == TokStart
                          then True
                      else 
                          True

isJumpStatement tok = if (tok !! 1) == TokJump
                          then True
                      else
                          True

isStopStatement tok = if (tok !! 1) == TokStop
                          then True
                      else 
                          True


{- #########################primitive function ############################################### -}
getFirstLine [] = []
getFirstLine (x:xs)
    | x == TokEndLine = [] 
    | x /= TokEndLine = x: getFirstLine xs


getSecLine [] _ = []
getSecLine toke n 
    | [] == result = []
    | otherwise = result
    where result = takeWhile (/=TokEndLine) $ drop 1 $ dropWhile (/=TokEndLine) toke

getIndent [] = (-1)
getIndent line = indent
        where (TokIndent a) = line !! 0
              indent = a

getRidIndent [] = []
getRidIndent line = drop 1 line

{- ##############################################################################################--}
getStartStatement tok = 
    if flag == "startStatement"
       then result
    else if flag == "confusion"
       then getStartStatement (result ++ proTok)
    else -- flag == "lastLine"
       result
    where l1 = getFirstLine tok
          l2 = getSecLine tok 0
          (result, flag) = compareStartLine l1 l2
          len = length result
          proTok = drop len tok

compareStartLine l1 l2 = 
  if indent1 /= -1 && indent2 == (-1)
      then (l1' ++ [TokPad, TokEndStatement], "lastLine")
  else if indent1 < indent2 
      then (l1 ++ l2' ++ [TokPad, TokPad, TokEndLine], "confusion")
  else if indent1 == indent2  && indent1 > 0
      then (l1 ++ l2' ++ [TokPad, TokPad, TokEndLine], "confusioin")
  else  -- indent1 > indent2
      (l1' ++ [TokPad, TokEndStatement], "startStatement")
  where indent1 = getIndent l1
        indent2 = getIndent l2
        l1' = getRidIndent l1
        l2' = getRidIndent l2

getWalkStatement tok =
    if flag == "walkStatement"
       then result
    else if flag == "confusion"
       then getWalkStatement (result ++ proTok)
    else  -- flag == "lastLine"
       result
    where l1 = getFirstLine tok
          l2 = getSecLine tok 0
          (result, flag) = compareWalkLine l1 l2
          len = length result
          proTok = drop len tok

compareWalkLine l1 l2 =
    if indent1 /= -1 && indent2 == (-1)
       then (l1' ++ [TokPad, TokEndStatement], "lastLine")
    else if indent1 < indent2
       then (l1 ++ l2' ++ [TokPad, TokPad, TokEndLine], "confusion")
    else if indent1 == indent2 && indent1 > 0
       then (l1 ++ l2' ++ [TokPad, TokPad, TokEndLine], "confusion")
    else -- indent1 > indent2
        (l1' ++ [TokPad, TokEndStatement], "walkStatement")
    where indent1 = getIndent l1
          indent2 = getIndent l2
          l1' = getRidIndent l1
          l2' = getRidIndent l2


getJumpStatement tok = 
  if flag == "jumpStatement" 
     then result
  else if flag == "confusion"
     then getJumpStatement (result ++ proTok)
  else -- flag == "lastLine"
     result
  where l1 = getFirstLine tok
        l2 = getSecLine tok 0
        (result, flag) = compareJumpLine l1 l2
        len = length result
        proTok = drop len tok

compareJumpLine l1 l2 =
    if indent1 /= -1 && indent2 == (-1)
        then (l1' ++ [TokPad, TokEndStatement], "lastLine")
    else if indent1 < indent2
        then (l1 ++ l2' ++ [TokPad, TokPad, TokEndLine], "confusion")
    else if indent1 == indent2 && indent1 >0
        then (l1 ++ l2' ++ [TokPad, TokPad, TokEndLine], "confusion")
    else  -- indent1 > indent2
        (l1' ++ [TokPad, TokEndStatement], "jumpStatement")
    where indent1 = getIndent l1
          indent2 = getIndent l2
          l1' = getRidIndent l1
          l2' = getRidIndent l2

getStopStatement tok =
    if flag == "stopStatement"
       then result
    else if flag == "confusion"
       then getStopStatement (result ++ proTok)
    else -- flag == "lastLine"
       result
    where l1 = getFirstLine tok
          l2 = getSecLine tok 0
          (result, flag) = compareStopLine l1 l2
          len = length result
          proTok = drop len tok

compareStopLine l1 l2 =
    if indent1 /= -1 && indent2 == (-1)
        then (l1' ++ [TokPad, TokEndStatement], "lastLine")
    else if indent1 < indent2
        then (l1 ++ l2' ++ [TokPad, TokPad, TokEndLine], "confusion")
    else if indent1 == indent2 && indent1 >0
        then (l1 ++ l2' ++ [TokPad, TokPad, TokEndLine], "confusion")
    else  -- indent1 > indent2
        (l1' ++ [TokPad, TokEndStatement], "stopStatement")
    where indent1 = getIndent l1
          indent2 = getIndent l2
          l1' = getRidIndent l1
          l2' = getRidIndent l2

{- ####################################### parse token ################--}

-- Wrapper for the `token` function in the Parsec library.
acceptToken :: (Token -> Maybe a) -> Parsec [Token] u a
acceptToken f =
  token show (const $ newPos "" 0 0) f

-- Given a token, generates a parser which accepts that token.
isToken :: Token -> Parsec [Token] u Token
isToken tok = acceptToken (\x -> if x == tok then Just x else Nothing)


oParen = isToken TokOParen
cParen = isToken TokCParen
comma = isToken TokComma
colon = isToken TokColon
endStmt = isToken TokEndStatement
equ = isToken TokEqu

pOn = isToken TokOn

--pWalk = isToken TokWalk
pStart = isToken TokStart
--pJump = isToken TokJump

pStop = isToken TokStop
pFall = isToken TokFall
pHitground = isToken TokHitground
pPosition = isToken TokPosition
pVelo = isToken TokVelo
pAcc = isToken TokAcc
pHitblock = isToken TokHitblock
pStep = isToken TokStep
pHitbonus = isToken TokHitbonus
pHitsignal = isToken TokHitsignal
pRed = isToken TokRed
pYellow = isToken TokYellow
pGreen = isToken TokGreen
pSignal = isToken TokSignal

pWalkRight = isToken TokWalkRight
pWalkRightSlow = isToken TokWalkRightSlow
pWalkRightFast = isToken TokWalkRightFast
pWalkLeft = isToken TokWalkLeft
pWalkLeftSlow = isToken TokWalkLeftSlow
pWalkLeftFast = isToken TokWalkLeftFast
pJumpUp = isToken TokJumpUp
pJumpUpLow = isToken TokJumpUpLow
pJumpUpHigh = isToken TokJumpUpHigh
pJumpRight = isToken TokJumpRight
pJumpRightLow = isToken TokJumpRightLow
pJumpRightHigh = isToken TokJumpRightHigh
pJumpLeft = isToken TokJumpLeft
pJumpLeftLow = isToken TokJumpLeftLow
pJumpLeftHigh = isToken TokJumpLeftHigh
pStand = isToken TokStand

name = acceptToken f
  where f (TokVar str) = Just str
        f _             = Nothing
int = acceptToken f
  where f (TokInt i) = Just i
        f _          = Nothing
indent = acceptToken f
  where f (TokIndent i) = Just i
        f _             = Nothing
