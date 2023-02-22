-- CPSC 312 Haskell Project: A Pokemon Battle
{-
    Authors: Justin Jao, Angela Li, Michael Cheung
-}

import System.IO
import Text.Read 

import Pokemon
import Heal

import Data.Maybe
import Data.List

-- State of the battle; the first object will always be the person player's Pokemon
type BattleState = (Pokemon, Pokemon)

-- Main function to start game
play :: IO ()
play = 
    do
        putStrLn "--------------------------------------"
        putStrLn "Welcome to the Pokemon Battle Arena!"
        putStrLn "Choose a Pokemon to use, wrapped in quotation marks:"
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
                                        let chosenMove = getMoveName move
                                        if chosenMove == "NULL"
                                            then 
                                                personBattle bs
                                            else
                                                do
                                                    putStrLn (getName (fst bs) ++ " used " ++ chosenMove ++ "!")
                                                    computerBattle (fst bs, useMoveOn move (snd bs))
                            else if action == "Item"
                                then
                                    do
                                        heal <- pollHeal (fst bs)
                                        let chosenHeal = getHealName heal
                                        if chosenHeal == "NULL"
                                            then
                                                personBattle bs
                                            else
                                                do
                                                    putStrLn ("Used a " ++ chosenHeal ++ " on " ++ getName (fst bs) ++ "!")
                                                    computerBattle (useHealOn heal (fst bs), snd bs)
                            else
                                personBattle bs

-- Continuously ask for a move
pollMove :: Pokemon -> IO Move
pollMove p = 
    do
        let moves = getMoves p
            moveNames = getMoveNames moves
        putStrLn ("Which move should " ++ getName p ++ " use? (leave empty to cancel)")
        print moveNames
        moveChoice <- getLine
        case (readMaybe moveChoice :: Maybe String) of
            Nothing -> return nullMove
            Just moveChoice ->
                if moveChoice `elem` moveNames
                    then
                        do
                        -- TODO: actual move not just default
                            let moveToUse = fromMaybe 0 (findIndex (== moveChoice) moveNames) in
                                return (moves !! moveToUse)
                    else
                        do
                            putStrLn ("Your " ++ getName p ++ "doesn't know that move!")
                            pollMove p

-- Continuously ask for the item to use
pollHeal :: Pokemon -> IO Heal
pollHeal p = 
    do
        let heals = allHealNames
        putStrLn ("Which item should be used on " ++ getName p ++ "? (leave empty to cancel)")
        print heals
        healChoice <- getLine
        case (readMaybe healChoice :: Maybe String) of
            Nothing -> return nullHeal
            Just healChoice ->
                if healChoice `elem` heals
                    then
                        return (getHealByName healChoice allHeals)
                    else
                        do
                            putStrLn "That item cannot be used!"
                            pollHeal p

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
                    let currHealth = getHealth (snd bs)
                        moveDmgs = getMoveDamages (getMoves (snd bs))
                        moves = getMoves (snd bs) in
                        if currHealth <= 20
                            then
                                do
                                    let 
                                    putStrLn ("The opponent used a Potion!")
                                    personBattle (fst bs, useHealOn (getHealByName "Potion" allHeals) (snd bs))
                            else
                                -- Will never use default value as maximum value of integers must exist
                                let moveToUse = fromMaybe 0 (findIndex (== maximum moveDmgs) moveDmgs) in
                                    do
                                        putStrLn ("The opponent's " ++ getName (snd bs) ++ " used " ++ getMoveName (moves !! moveToUse))
                                        personBattle (useMoveOn (moves !! moveToUse) (fst bs), snd bs)