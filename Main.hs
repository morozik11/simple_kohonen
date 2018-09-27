{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

newtype Vector a = Vector [a] deriving (Show,Read,Ord,Eq)

newtype Vectors v a = Vectors [v a] deriving (Show,Read,Ord,Eq)

class Normalize v a | v a -> a where
  wrapGetMax ::  v a -> Maybe a
  wrapGetMin :: v a -> Maybe a

instance (Ord a, Num a) => Normalize Vector a where
  --------------------------------
  wrapGetMax (Vector []) = Nothing
  wrapGetMax (Vector xs) =
    let max = maximum xs
    in Just max
  ---------------------------------
  wrapGetMin (Vector []) = Just 0
  wrapGetMin (Vector xs) =
    let min = minimum xs
    in Just min


instance (Normalize v a,Ord a, Num a) => Normalize (Vectors v) a where
  ---------------------------------
  wrapGetMax (Vectors [])  = Nothing
  wrapGetMax (Vectors vs) =
    let maxList =  map wrapGetMax vs
    in getMaxMinMaybe maxList maximum
  ---------------------------------
  wrapGetMin (Vectors []) = Nothing
  wrapGetMin (Vectors vs) =
    let minList = map wrapGetMin vs
    in getMaxMinMaybe minList minimum
  

getMaxMinMaybe :: (Ord a, Num a) => [Maybe a] -> ([Maybe a] -> Maybe a) -> Maybe a
getMaxMinMaybe [Nothing] _  = Nothing
getMaxMinMaybe (xs) f  = f xs

class GetAGetB v a | v a -> a where
  getA :: v a -> Maybe a
  --getb :: v a -> Maybe a



instance (GetAGetB v a, Normalize v a,  Ord a, Num a, Fractional a) => GetAGetB (Vectors v) a where
  --------------------------------
  getA vs =
    wrapGeta max min
    where
      max = wrapGetMax vs
      min = wrapGetMin vs




wrapGeta :: (Ord a, Num a, Fractional a) => Maybe a -> Maybe a -> Maybe a
wrapGeta (Nothing) (Just min)  = Just 0
wrapGeta (Just max) (Nothing) = Just 0
wrapGeta (Just max) (Just min) =
  let expression =  max - min 
  in Just (1/expression)

{--

a = 1 / max - min
b = -min / max - min

v = a * x + b
w ~ R[0.1 - 0.3]

y = 0.3 , Dy = 0.05

--}

main :: IO()
main = do
    return ()
