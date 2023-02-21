module Pokemon
(
    Pokemon,
    Move,
    allPokemonNames,
    listMoves,
) where

-- The amount of damage a particular move inflicts
type Power = Int

-- Constructor for a Move, it has a name and an amount of damage
data Move = Move String Power

-- A Pokemon has a name, health remaining, and up to 4 moves
data Pokemon = Pokemon4 { name :: String, health :: Int, move1 :: Move, move2 :: Move, move3 :: Move, move4 :: Move }
             | Pokemon3 { name :: String, health :: Int, move1 :: Move, move2 :: Move, move3 :: Move }
             | Pokemon2 { name :: String, health :: Int, move1 :: Move, move2 :: Move }
             | Pokemon1 { name :: String, health :: Int, move1 :: Move }

-- Define Pokemon constants
bulbasaur = Pokemon2 "Bulbasaur" 90 (Move "Tackle" 20) (Move "Vine Whip" 20)
squirtle = Pokemon2 "Squirtle" 80 (Move "Tackle" 20) (Move "Water Gun" 20)
charmander = Pokemon2 "Charmander" 80 (Move "Tackle" 20) (Move "Ember" 20)

-- Create Pokemon "library"
allPokemon :: [Pokemon]
allPokemon = [bulbasaur, charmander, squirtle]

-- Helper function: add Pokemon name to library listing
addPokemonName :: [String] -> Pokemon -> [String]
addPokemonName [] p = [name p]
addPokemonName lst p = lst ++ [name p]

-- Provide all Pokemon names available for use
allPokemonNames :: [String]
allPokemonNames = foldl addPokemonName [] allPokemon

-- Helper function: determine the moves available on this Pokemon
listMoves :: Pokemon -> [String]
listMoves (Pokemon1 _ _ (Move n1 _)) = [n1]
listMoves (Pokemon2 _ _ (Move n1 _) (Move n2 _)) = [n1, n2]
listMoves (Pokemon3 _ _ (Move n1 _) (Move n2 _) (Move n3 _)) = [n1, n2, n3]
listMoves (Pokemon4 _ _ (Move n1 _) (Move n2 _) (Move n3 _) (Move n4 _)) = [n1, n2, n3, n4]