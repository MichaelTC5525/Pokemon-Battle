module Heal 
(
    Heal,
    nullHeal,
    allHeals,
    allHealNames,
    getHealByName,
    getHealName,
    getHealAmount,
) where

-- A healing item has a name and the amount of health it restores
data Heal = Heal { healName :: String, healAmount :: Int }

-- Define Healing item constants
potion = Heal "Potion" 20
superPotion = Heal "Super Potion" 50

-- Constant Null Heal, used to cancel item usage
nullHeal = Heal "NULL" 0

-- Healing item "catalogue"
allHeals = [potion, superPotion]

-- Helper function for allHealNames
addHealName :: [String] -> Heal -> [String]
addHealName [] h = [healName h]
addHealName lst h = lst ++ [healName h]

-- Provide all Heal names available for use
allHealNames :: [String]
allHealNames = foldl addHealName [] allHeals

-- Retrieve the details for a Heal by its name
getHealByName :: String -> [Heal] -> Heal
getHealByName s lst
    | s == healName (head lst) = head lst
    | otherwise = getHealByName s (tail lst)

-- Getters for Heal records
getHealName :: Heal -> String
getHealName = healName

getHealAmount :: Heal -> Int
getHealAmount = healAmount
