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

--main = do 
--	args <- getArgs
--	let fileName = head args
--	processFile fileName setting1

processFile fileName setting = do
	handle <- openFile fileName ReadMode
	a <- hGetContents handle
	let lower = toLowerS a
	let result = process lower
	putStrLn $ show result
	hClose handle
	runStateT (animate result setting) ([], 0)


process input = convertS statement
    where Right statement = parse program "?" tok
          Right tok = parse lexer "?" input

toLowerS :: String -> String
toLowerS [] = []
toLowerS (x:xs) = toLower x : toLowerS xs

t0 = processFile "scripts/simplejump.src" setting0
t1 = processFile "scripts/jumpover.src" setting1
t2 = processFile "scripts/bounce.src" setting2
t3 = processFile "scripts/climbup.src" setting3
t4 = processFile "scripts/jumptoblock.src" setting4
t5 = processFile "scripts/signal.src" setting5

t00 = processFile "scripts/script11.src" setting11
t01 = processFile "scripts/script12.src" setting12