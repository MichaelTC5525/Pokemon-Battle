module Moves 
(
    Move(Move),
    getMoveName,
    getMoveDamage,
    nullMove,
) where

-- The amount of damage a particular move inflicts
type Power = Int

-- Constructor for a Move, it has a name and an amount of damage
data Move = Move { moveName :: String, damage :: Power }

-- Constant Null Move, used for cancelling decision to use a Move
nullMove = Move "NULL" 0

-- "Public" getters
getMoveName :: Move -> String
getMoveName = moveName

getMoveDamage :: Move -> Power
getMoveDamage = damage