module Main where

import qualified System.Console.ANSI as Console
import           Text.Parsec

import GameBoard
import Pos
import Vessel

main :: IO ()
main = do
  putStrLn "Generating board ..."
  gb <- generateBoard
  print gb
