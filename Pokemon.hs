module Pokemon
(
    Pokemon,
    Move,
    allPokemon,
    allPokemonNames,
    getPokemonByName,
    getName,
    getHealth,
    getMoveName,
    nullMove,
    getMoveNames,
    getMoveDamages,
    getMoves,
    useMoveOn,
    useHealOn,
) where

import Heal
import Moves

-- A Pokemon has a name, health remaining, and up to 4 moves
data Pokemon = Pokemon1 { name :: String, health :: Int, move1 :: Move }
             | Pokemon2 { name :: String, health :: Int, move1 :: Move, move2 :: Move }
             | Pokemon3 { name :: String, health :: Int, move1 :: Move, move2 :: Move, move3 :: Move } 
             | Pokemon4 { name :: String, health :: Int, move1 :: Move, move2 :: Move, move3 :: Move, move4 :: Move }

-- Define Pokemon constants
bulbasaur = Pokemon2 "Bulbasaur" 90 (Move "Tackle" 20) (Move "Vine Whip" 25)
squirtle = Pokemon2 "Squirtle" 80 (Move "Tackle" 20) (Move "Water Gun" 25)
charmander = Pokemon2 "Charmander" 80 (Move "Tackle" 20) (Move "Ember" 25)
pikachu = Pokemon3 "Pikachu" 90 (Move "Tackle" 20) (Move "Thunderbolt" 30) (Move "Thundershock" 20)

-- Create Pokemon "library"
allPokemon :: [Pokemon]
allPokemon = [bulbasaur, charmander, squirtle, pikachu]

-- Helper function: add Pokemon name to library listing
addPokemonName :: [String] -> Pokemon -> [String]
addPokemonName [] p = [name p]
addPokemonName lst p = lst ++ [name p]

-- Provide all Pokemon names available for use
allPokemonNames :: [String]
allPokemonNames = foldl addPokemonName [] allPokemon

-- Based on a name, retrieve the Pokemon entity
getPokemonByName :: String -> [Pokemon] -> Pokemon
getPokemonByName s lst
    | s == name (head lst) = head lst
    | otherwise = getPokemonByName s (tail lst)

-- Simple "public" getter functions
getName :: Pokemon -> String
getName = name

getHealth :: Pokemon -> Int
getHealth = health

-- Getter functions for inner move fields
getMoveNames :: [Move] -> [String]
getMoveNames ms = [getMoveName m | m <- ms]

getMoveDamages :: [Move] -> [Int]
getMoveDamages ms = [getMoveDamage m | m <- ms]

-- Getter function: for when you need the "objects" alongside their associated properties
getMoves :: Pokemon -> [Move]
getMoves (Pokemon1 _ _ m1) = [m1]
getMoves (Pokemon2 _ _ m1 m2) = [m1, m2]
getMoves (Pokemon3 _ _ m1 m2 m3) = [m1, m2, m3]
getMoves (Pokemon4 _ _ m1 m2 m3 m4) = [m1, m2, m3, m4]

-- Simulate a Pokemon taking the effects of a Move
-- PRECONDITIONS: a Pokemon passed to this function is known to initially have a health value > 0
useMoveOn :: Move -> Pokemon -> Pokemon
useMoveOn m (Pokemon1 n h m1) = Pokemon1 n (h - getMoveDamage m) m1
useMoveOn m (Pokemon2 n h m1 m2) = Pokemon2 n (h - getMoveDamage m) m1 m2
useMoveOn m (Pokemon3 n h m1 m2 m3) = Pokemon3 n (h - getMoveDamage m) m1 m2 m3
useMoveOn m (Pokemon4 n h m1 m2 m3 m4) = Pokemon4 n (h - getMoveDamage m) m1 m2 m3 m4

-- Simulate a Pokemon taking the effects of a Heal
-- PRECONDITIONS: a Pokemon passed to this function is known to initially have a health value > 0
-- TODO: limit to a max health
useHealOn :: Heal -> Pokemon -> Pokemon
useHealOn i (Pokemon1 n h m1) = Pokemon1 n (h + getHealAmount i) m1
useHealOn i (Pokemon2 n h m1 m2) = Pokemon2 n (h + getHealAmount i) m1 m2
useHealOn i (Pokemon3 n h m1 m2 m3) = Pokemon3 n (h + getHealAmount i) m1 m2 m3
useHealOn i (Pokemon4 n h m1 m2 m3 m4) = Pokemon4 n (h + getHealAmount i) m1 m2 m3 m4
