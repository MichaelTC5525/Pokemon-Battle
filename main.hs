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
play :: IO ()
play = 
    do
        putStrLn "--------------------------------------"
        putStrLn "Welcome to the Pokemon Battle Arena!"
        putStrLn "Choose a Pokemon to use, wrapped in quotation marks, including quotation marks:"
        putStrLn "(leave empty to exit, or \"list\" for available Pokemon)"
        choice <- getLine 
        case (readMaybe choice :: Maybe String) of
            Nothing -> putStrLn "We hope to see you again!"
            Just choice ->
                if choice == "list"
                    then
                        do
                            print allPokemonNames
                            play
                else if choice `elem` allPokemonNames
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

-- Helper function to ensure whether the state of the game is continuing or will complete
checkBattleState :: BattleState -> IO Bool
checkBattleState bs =
    let maxHealth1 = getHealth (fst bs)
        maxHealth2 = getHealth (snd bs)
    in
    do
        if (maxHealth1 <= 0) && (maxHealth2 <= 0)
            then
                do
                    putStrLn "It's a draw!"
                    return True
        else if maxHealth1 <= 0
            then
                do
                    putStrLn "You lost!"
                    return True
        else if maxHealth2 <= 0
            then
                do
                    putStrLn "You won!"
                    return True
        else
            return False

-- Player's turn to perform an action
personBattle :: BattleState -> IO ()
personBattle bs =
    do
        currResult <- checkBattleState bs
        if currResult
            then
                putStrLn "The battle has ended."
            else
                -- TODO: poll actions from player
                do
                    putStrLn "--------------------------------"
                    putStrLn ("Your " ++ getName (fst bs) ++ " has " ++ show (getHealth (fst bs)) ++ " health.")
                    putStrLn ("Your opponent's " ++ getName (snd bs) ++ " has " ++ show (getHealth (snd bs)) ++ " health.")
                    putStrLn "[\"Move\"] Use a Pokemon move / [\"Item\"] Use an item"
                    action <- getLine
                    case (readMaybe action :: Maybe String) of
                        Nothing -> personBattle bs
                        Just action ->
                            if action == "Move"
                                then
                                    do
                                        move <- pollMove (fst bs)
                                        if getMoveName move == "NULL"
                                            then 
                                                personBattle bs
                                            else
                                                do
                                                    putStrLn (getName (fst bs) ++ " used " ++ getMoveName move ++ "!")
                                                    -- TODO: update battlestate
                                                    computerBattle (fst bs, useMoveOn move (snd bs))
                            -- else if action == "Item"
                            --     then
                            --         pollItem
                            else
                                personBattle bs

-- Continuously ask for a move
-- TODO: if possible, allow for people to change their mind i.e. go back to Move or Item
pollMove :: Pokemon -> IO Move
pollMove p = 
    do
        let moves = getMoves p
        putStrLn ("Which move should " ++ getName p ++ " use? (leave empty to cancel)")
        print moves
        moveChoice <- getLine
        case (readMaybe moveChoice :: Maybe String) of
            Nothing -> return nullMove
            Just moveChoice ->
                if moveChoice `elem` moves
                    then
                        -- TODO: actual move not just default
                        return (getMoveByName moveChoice p)
                    else
                        pollMove p

-- pollItem :: IO ()
-- pollItem = return

-- Computer's turn, less involved action from user necessary
computerBattle :: BattleState -> IO ()
computerBattle bs =
    do
        currResult <- checkBattleState bs
        if currResult
            then
                putStrLn "The battle has ended."
            else
                do
                    -- TODO: poll actions from computer
                    putStrLn ("The opponent's " ++ getName (snd bs) ++ " used NULL")
                    personBattle bs