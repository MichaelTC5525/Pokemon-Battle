module Heal where

-- A healing item has a name and the amount of health it restores
data Heal = Heal { healName :: String, healAmount :: Int }

-- Define Healing item constants
potion = Heal "Potion" 20
superPotion = Heal "Super Potion" 50

getHealName :: Heal -> String
getHealName = healName

getHealAmount :: Heal -> Int
getHealAmount = healAmount


