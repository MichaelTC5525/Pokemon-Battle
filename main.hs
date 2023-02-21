-- CPSC 312 Haskell Project: A Pokemon Battle
{-
    Authors: Justin Jao, Angela Li, Michael Cheung
-}

import System.IO
import Text.Read 

import Pokemon

-- A healing item has a name and the amount of health it restores
data Heal = Heal String Int

-- On each turn, either we perform a move or use a healing item
type BattleAction = Move
type HealthAction = Heal

-- State of the battle
type BattleState = (Pokemon, Pokemon)

-- Define Healing item constants
potion = Heal "Potion" 20
superPotion = Heal "Super Potion" 50

-- Main function to start game
play :: IO () -- TODO: update type
play = 
    do
        putStrLn "--------------------------------------"
        putStrLn "Welcome to the Pokemon Battle Arena"
        putStrLn "Choose a Pokemon to use, wrapped in quotation marks, including quotation marks:"
        putStrLn "(leave empty to exit, or \"list\" for available Pokemon)"
        choice <- getLine 
        case (readMaybe choice :: Maybe String) of
            Nothing -> 
                do
                    putStrLn "Goodbye"
            -- TODO: list Pokemon names when "list"
            Just choice ->
                if choice `elem` allPokemonNames
                    then
                        do
                            putStrLn ("You chose " ++ choice)
                    else
                        do
                            putStrLn "Try again"
                            play
