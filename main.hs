-- CPSC 312 Haskell Project: A Pokemon Battle
{-
    Authors: Justin Jao, Angela Li, Michael Cheung
-}

import System.IO
import Text.Read 

-- Define our datatypes iteratively, small to big

-- The amount of damage a particular move inflicts
type Power = Int

-- Constructor for a Move, it has a name and an amount of damage
data Move = Move String Power

-- A Pokemon has a name, health remaining, and up to 4 moves
data Pokemon = Pokemon4 String Int Move Move Move Move
             | Pokemon3 String Int Move Move Move
             | Pokemon2 String Int Move Move
             | Pokemon1 String Int Move

-- A healing item has a name and the amount of health it restores
data Heal = Heal String Int

-- On each turn, either we perform a move or use a healing item
type BattleAction = Move
type HealthAction = Heal

-- State of the battle
type BattleState = (Pokemon, Pokemon)

-- Main function to start game
play :: IO String -- TODO: update type
play = 
    do
        putStrLn "Welcome to the Pokemon Battle Arena"
        putStrLn "Choose a starter Pokemon, including quotation marks: \"Bulbasaur\", \"Squirtle\", \"Charmander\" (leave empty to exit)"
        choice <- getLine 
        case (readMaybe choice :: Maybe String) of
            Nothing -> return "Goodbye"
            Just choice ->
                if choice `elem` ["Bulbasaur", "Squirtle", "Charmander"]
                    then
                        do
                            return ("You chose " ++ choice)
                    else
                        do
                            putStrLn "Try again"
                            play