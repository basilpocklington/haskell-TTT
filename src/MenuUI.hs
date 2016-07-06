module MenuUI where

import UI
import System.IO
import Text.Regex.Posix

welcomeMessage :: String
welcomeMessage = clearScreenHome ++ "Welcome To Haskell Tic Tac Toe!"

menuPromptMessage :: IO ()
menuPromptMessage = do
  putStrLn welcomeMessage
  putStrLn "1) Human vs Human"
  putStrLn "2) Human vs Computer"
  putStrLn "3) Computer vs Computer"
  putStr "Please choose a game type(1-3): "

inputMenuPrompt :: IO String
inputMenuPrompt = do
  menuPromptMessage
  hFlush stdout
  getLine

isValidMenuInput :: String -> Bool
isValidMenuInput userInput = userInput =~ "^[1-3]$"

getUserMenuInput :: IO String -> IO String
getUserMenuInput menuPrompt = do
  input <- menuPrompt
  if isValidMenuInput input
    then return input
    else getUserMenuInput inputMenuPrompt

