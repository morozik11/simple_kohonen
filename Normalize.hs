{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Normalize where

newtype Vector a = Vector [a] deriving (Show,Read,Ord,Eq)

newtype Vectors v a = Vectors [Vector a] deriving (Show,Read,Ord,Eq)

class WrapGetMaxGetMin v a where
  wrapGetMax ::  v a -> a
  wrapGetMin :: v a -> a


instance (Ord a, Num a) => WrapGetMaxGetMin Vector a where
  wrapGetMax (Vector []) = 0
  wrapGetMax (Vector v) = maximum v
  -------------------------------
  wrapGetMin (Vector []) = 0
  wrapGetMin (Vector v) = minimum v


instance (Ord a, Num a) => WrapGetMaxGetMin (Vectors v) a  where
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


class Normalize v a where
  normalize ::  v a -> v a 


instance (Ord a, Num a, Fractional a) => Normalize (Vectors v) a where
  normalize (Vectors []) = Vectors []
  normalize (Vectors vs) =
    let a = getA (Vectors vs)
        b = getB (Vectors vs)
    in
      setNormalElemVectors (Vectors vs) a b


--------------------------------------------


setNormalElemVectors :: (Ord a, Num a, Fractional a) =>  (Vectors v) a -> a -> a -> (Vectors v) a
setNormalElemVectors (Vectors []) _ _  = Vectors []
setNormalElemVectors (Vectors vs) a b  =
  let filteredList = filterEmpty (Vectors vs)
  in
    let listVectors = map (\x -> setNormalElemVector x a b) filteredList
    in (Vectors listVectors)

setNormalElemVector :: (Ord a, Num a, Fractional a) => Vector a -> a -> a -> Vector a
setNormalElemVector (Vector []) _ _  =  Vector []
setNormalElemVector (Vector v) a b  =
  let listVector =  map (\x -> x * a + b)  v
  in (Vector listVector)

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

