module Run where

import Command
import Text.Parsec
import Parser
import Lexer
import Data.Char
import System.Environment
import System.IO
import Game
import Control.Monad.Trans.State
import Data.Maybe
--main = do 
--	args <- getArgs
--	let fileName = head args
--	processFile fileName setting1
--processFile :: Maybe String -> StateT Env IO ()
processFile fileName setting = do
	if not $ isNothing fileName 
		then do
			handle <- openFile (fromJust fileName) ReadMode
			a <- hGetContents handle
			let lower = toLowerS a
			let result = process lower
			putStrLn $ show result
			hClose handle
			runStateT (animate result setting) ([], 0)
		else 
			runStateT (animateKeyboard setting) ([], 0)



process input = convertS statement
    where Right statement = parse program "?" tok
          Right tok = parse lexer "?" input

toLowerS :: String -> String
toLowerS [] = []
toLowerS (x:xs) = toLower x : toLowerS xs

t0 = processFile (Just "scripts/simplejump.src") setting0
t1 = processFile (Just "scripts/jumpover.src") setting1
t2 = processFile (Just "scripts/bounce.src") setting2
t3 = processFile (Just "scripts/climbup.src") setting3
t4 = processFile (Just "scripts/jumptoblock.src") setting4
t5 = processFile (Just "scripts/signal.src") setting5
t6 = processFile (Just "scripts/test6.src") setting6

t00 = processFile (Just "scripts/script11.src") setting11
t01 = processFile (Just "scripts/script12.src") setting12