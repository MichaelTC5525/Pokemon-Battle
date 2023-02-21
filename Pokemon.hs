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
    getMoveDamage,
    nullMove,
    getMoveByName,
    getMoves,
    useMoveOn,
) where

-- The amount of damage a particular move inflicts
type Power = Int

-- Constructor for a Move, it has a name and an amount of damage
data Move = Move { moveName :: String, damage :: Power }
            deriving (Show)

-- "Public" getters
getMoveName :: Move -> String
getMoveName = moveName

getMoveDamage :: Move -> Power
getMoveDamage = damage

-- Constant Null Move, used for cancelling decision to use a Move
nullMove = Move "NULL" 0

-- A Pokemon has a name, health remaining, and up to 4 moves
data Pokemon = Pokemon1 { name :: String, health :: Int, move1 :: Move }            
             | Pokemon2 { name :: String, health :: Int, move1 :: Move, move2 :: Move }
             | Pokemon3 { name :: String, health :: Int, move1 :: Move, move2 :: Move, move3 :: Move } 
             | Pokemon4 { name :: String, health :: Int, move1 :: Move, move2 :: Move, move3 :: Move, move4 :: Move }
             deriving (Show)

-- Define Pokemon constants
bulbasaur = Pokemon2 "Bulbasaur" 90 (Move "Tackle" 20) (Move "Vine Whip" 20)
squirtle = Pokemon2 "Squirtle" 80 (Move "Tackle" 20) (Move "Water Gun" 20)
charmander = Pokemon2 "Charmander" 80 (Move "Tackle" 20) (Move "Ember" 20)
pikachu = Pokemon3 "Pikachu" 90 (Move "Tackle" 20) (Move "Thunderbolt" 30) (Move "Thundershock" 25)

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

getMoveByName :: String -> Pokemon -> Move
-- TODO: which move to use
getMoveByName s = move1

-- Getter function: determine the moves available on this Pokemon
getMoves :: Pokemon -> [String]
getMoves (Pokemon1 _ _ (Move n1 _)) = [n1]
getMoves (Pokemon2 _ _ (Move n1 _) (Move n2 _)) = [n1, n2]
getMoves (Pokemon3 _ _ (Move n1 _) (Move n2 _) (Move n3 _)) = [n1, n2, n3]
getMoves (Pokemon4 _ _ (Move n1 _) (Move n2 _) (Move n3 _) (Move n4 _)) = [n1, n2, n3, n4]

-- Simulate a Pokemon taking the effects of a Move
useMoveOn :: Move -> Pokemon -> Pokemon
useMoveOn m (Pokemon1 n h m1) = Pokemon1 n (h - getMoveDamage m) m1
useMoveOn m (Pokemon2 n h m1 m2) = Pokemon2 n (h - getMoveDamage m) m1 m2
useMoveOn m (Pokemon3 n h m1 m2 m3) = Pokemon3 n (h - getMoveDamage m) m1 m2 m3
useMoveOn m (Pokemon4 n h m1 m2 m3 m4) = Pokemon4 n (h - getMoveDamage m) m1 m2 m3 m4
