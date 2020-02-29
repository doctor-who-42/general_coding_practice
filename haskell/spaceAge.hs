{-
Exercism's age-calculating problem:
Given an age in seconds, calculate how old someone would be
 on each planet for which you are given their orbital period
-}

module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune
    deriving Eq

eSec :: Float
eSec = 31557600

orbP :: Planet -> Float
orbP Mercury = 0.2408467
orbP Venus   = 0.61519726
orbP Earth   = 1
orbP Mars    = 1.8808158
orbP Jupiter = 11.862615
orbP Saturn  = 29.447498
orbP Uranus  = 84.016846
orbP Neptune = 164.79132

ageOn :: Planet -> Float -> Float
ageOn p s = ageOnEarth s / orbP p

ageOnEarth :: Float -> Float
ageOnEarth = (/ eSec)
