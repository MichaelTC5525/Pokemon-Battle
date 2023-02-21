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
        putStrLn "Welcome to the Pokemon Battle Arena!"
        putStrLn "Choose a Pokemon to use, wrapped in quotation marks, including quotation marks:"
        putStrLn "(leave empty to exit, or \"list\" for available Pokemon)"
        choice <- getLine 
        case (readMaybe choice :: Maybe String) of
            Nothing -> putStrLn "We hope to see you again!"
            -- TODO: list Pokemon names when "list"
            Just choice ->
                if choice `elem` allPokemonNames
                    then
                        do
                            putStrLn "The Battle Begins!"
                            -- TODO: randomize computer choice of Pokemon
                            -- TODO: should we always have player start first?
                            personBattle (getPokemonByName choice allPokemon, getPokemonByName "Squirtle" allPokemon)
                    else
                        do
                            putStrLn "Not an available Pokemon name. Try again."
                            play

checkBattleState :: BattleState -> IO Bool
checkBattleState bs =
    let maxHealth1 = getHealth (fst bs)
        maxHealth2 = getHealth (snd bs)
    in
    do
        if (maxHealth1 == 0) && (maxHealth2 == 0)
            then
                do
                    putStrLn "It's a draw!"
                    return True
        else if maxHealth1 == 0
            then
                do
                    putStrLn "You lost!"
                    return True
        else if maxHealth2 == 0
            then
                do
                    putStrLn "You won!"
                    return True
        else
            return False




personBattle :: BattleState -> IO ()
personBattle bs =
    do
        currResult <- checkBattleState bs
        if currResult
            then
                putStrLn "The battle has ended."
            else
                -- TODO: poll actions from player
                putStrLn "hello world"


computerBattle :: BattleState -> IO ()
computerBattle bs =
    do
        currResult <- checkBattleState bs
        if currResult
            then
                putStrLn "The battle has ended."
            else
                -- TODO: poll actions from computer
                putStrLn "hello world"


                    
