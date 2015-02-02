#!/usr/bin/env runhaskell

{-|
Module: Main
Description: Sample standalone program
Copyright: Franklin Chen, 2015
License: BSD3
Maintainer: franklinchen@franklinchen.com

Sample standalone interactive console program that
reads from stdin and writes to stdout.

* Load this module from GHCi with
    @
      :l Main
    @

* doctest this module from GHCi with
    @
      :testMain
    @

* Play the game from GHCi by typing
    @
      main
    @

* Play the game from the command line as a script with
    @
      runhaskell src/Main.hs
    @

  or even

    @
      ./src/Main.hs
    @

* Play the game as a native-compiled binary with
    @
      cabal build game
    @

    @
      dist/build/game/game
    @
-}

module Main where

import qualified System.Random as Random

-- | 'IO' action that gives back a random 'Int' between 1 and 6.
rollDice :: IO Int
rollDice = Random.getStdRandom (Random.randomR (1,6))

-- | Entry point to the game.
main :: IO ()
main = do
  secret <- rollDice
  gameLoop secret


-- | Return 'Just' @n@ if the string parses to a valid die roll 1
-- through 6, else 'Nothing'
--
-- >>> parseGuess "5"
-- Just 5
--
-- >>> parseGuess "junk"
-- Nothing
--
-- >>> parseGuess "7"
-- Nothing
parseGuess :: String -> Maybe Int
parseGuess "1" = Just 1
parseGuess "2" = Just 2
parseGuess "3" = Just 3
parseGuess "4" = Just 4
parseGuess "5" = Just 5
parseGuess "6" = Just 6
parseGuess _ = Nothing

-- | Prompt the user for a guess. If the response is "quit", say goodbye
-- and exit.
--
-- If the response is a valid die roll and matches
-- the secret die roll, congratulate the user and exit.
-- If it is not even a valid die roll, admonish the user and continue.
-- Otherwise, tell the user the guess was wrong, and continue.
--
-- Use 'putStr', 'putStrLn', 'getLine', and 'parseGuess'.
gameLoop :: Int -> IO ()
gameLoop secret = do
  putStrLn "I have a secret number."
  putStr "Guess? "
  guessString <- getLine
  if guessString == "quit" then
    putStrLn "Quitter!"
  else
    case parseGuess guessString of
      Nothing -> do
        putStrLn "*** Invalid guess!"
        gameLoop secret
      Just i
        | i == secret -> putStrLn "Congratulations!"
        | otherwise -> do
            putStrLn "Sorry, you guessed wrong."
            gameLoop secret
