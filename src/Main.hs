module Main where

import qualified System.Console.ANSI as C
import System.IO
import Text.Parsec

import GameBoard
import Pos
import Vessel

runGame :: GameBoard -> IO ()
runGame board = do
  putStrLn $ renderBoard board
  if gameOver board
    then do
      putStrLn "Game Over"
      putStrLn "YOU WIN!!!"
    else do
      putStr "Take a shot (x,y): "
      line <- getLine
      C.clearScreen
      case parse posParser "" line of
        Left _ -> do
          putStrLn $ "Invalid input " ++ show line
          runGame board
        Right shot ->
          -- take the shot and bind the result back into the recursive call to runGame
          shoot board shot >>= runGame

shoot :: GameBoard -> Pos -> IO GameBoard
shoot board shot = do
  let result = takeShot board shot
  case result of
    OffBoard -> do
      putStrLn $ "Off board " ++ show shot
      pure board
    RepeatShot -> do
      putStrLn $ "Repeat shot " ++ show shot
      pure board
    ShotHit newBoard maybeSunk -> do
      printSunk maybeSunk
      putStrLn $ "HIT " ++ show shot
      pure newBoard
    ShotMiss newBoard -> do
      putStrLn $ "MISS " ++ show shot
      pure newBoard

printSunk :: Maybe VesselType -> IO ()
printSunk (Just vt) = putStrLn $ "You sunk my " ++ show vt ++ "!"
printSunk Nothing = pure ()

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  C.clearScreen
  putStrLn "Generating board ..."
  board <- generateBoard
  runGame board
