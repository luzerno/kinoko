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