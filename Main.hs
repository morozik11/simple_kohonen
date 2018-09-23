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
  wrapGetMin (Vector []) = Nothing
  wrapGetMin (Vector xs) =
    let min = minimum xs
    in Just min


instance (Normalize v a,Ord a, Num a) => Normalize (Vectors v) a where
  ---------------------------------
  wrapGetMax (Vectors [])  = Nothing
  wrapGetMax (Vectors vs) = getMaxMaybe (map wrapGetMax vs)
  ---------------------------------
  wrapGetMin (Vectors []) = Nothing
  wrapGetMin (Vectors vs) = getMinMaybe (map wrapGetMin vs)
  

getMaxMaybe :: (Ord a, Num a) => [Maybe a] -> Maybe a
getMaxMaybe [Nothing] = Nothing
getMaxMaybe (xs) = maximum xs

getMinMaybe :: (Ord a, Num a) => [Maybe a] -> Maybe a
getMinMaybe [Nothing] = Nothing
getMinMaybe (xs) = minimum xs


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
