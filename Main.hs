{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

newtype Vector a = Vector [a] deriving (Show,Read,Ord,Eq)

newtype Vectors v a = Vectors [Vector a] deriving (Show,Read,Ord,Eq)

class Normalize v a where
  wrapGetMax ::  v a -> a
  wrapGetMin :: v a -> a


instance (Ord a, Num a) => Normalize Vector a where
  wrapGetMax (Vector []) = 0
  wrapGetMax (Vector v) = maximum v
  -------------------------------
  wrapGetMin (Vector []) = 0
  wrapGetMin (Vector v) = minimum v


instance (Ord a, Num a) => Normalize (Vectors v) a  where
  wrapGetMax (Vectors []) = 0
  wrapGetMax (Vectors v) =
    let filteredList = filterEmpty (Vectors v)
    in
      let listVector = map wrapGetMax filteredList
      in maximum listVector
    
  -------------------------------
  wrapGetMin (Vectors []) = 0
  wrapGetMin (Vectors v) =
    let filteredList = filterEmpty (Vectors v)
    in
      let listVector = map wrapGetMin filteredList
      in minimum listVector


class GetA_B v a where
  getA :: v a -> a
  getB :: v a -> a
  getA_B :: v a -> (a,a)

instance (Ord a, Num a, Fractional a) => GetA_B (Vectors v) a where
  getA (Vectors []) = 0
  getA (Vectors vs) =
    let max = wrapGetMax (Vectors vs)
    in
      1 / (max - min)
      where min = wrapGetMin (Vectors vs)
  -------------------------------
  getB (Vectors []) = 0
  getB (Vectors vs) =
    let min = wrapGetMin (Vectors vs)
    in
      -min / (max - min)
      where max = wrapGetMax (Vectors vs)
   -------------------------------
  getA_B (Vectors []) = (0,0)
  getA_B (Vectors vs) = (getA (Vectors vs), getB (Vectors vs))



filterEmpty :: (Ord a, Num a) => (Vectors v) a  -> [Vector a]
filterEmpty (Vectors []) = []
filterEmpty (Vectors vs) =
  filter (\x -> x /= Vector []) vs



 

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
