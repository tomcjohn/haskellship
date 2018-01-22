module Main where

import qualified System.Console.ANSI as Console
import Text.Parsec

import GameBoard
import Pos

runGame :: GameBoard -> IO ()
runGame board = do
  printBoard board
  if gameOver board
    then do
      putStrLn "Game Over"
      putStrLn "YOU WIN!!!"
    else do
      putStr "Take a shot (x,y): "
      line <- getLine
      Console.clearScreen
      case parse posParser "" line of
        Left _ -> do
          putStrLn $ "Invalid input " ++ show line
          runGame board
        Right shot ->
          -- take the shot and bind the result back into the recursive call to runGame
          shoot board shot >>= runGame

shoot :: GameBoard -> Pos -> IO GameBoard
shoot board shot = do
  result <- takeShot board shot
  case result of
    OffBoard -> do
      putStrLn $ "Off board " ++ show shot
      pure board
    RepeatShot -> do
      putStrLn $ "Repeat shot " ++ show shot
      pure board
    Hit newVessels -> do
      putStrLn $ "HIT " ++ show shot
      pure $ board {vessels=newVessels}
    Miss -> do
      putStrLn $ "MISS " ++ show shot
      pure $ board {misses=Set.insert shot (misses board)}

main :: IO ()
main = do
  Console.clearScreen
  putStrLn "Generating board ..."
  board <- generateBoard
  runGame board
