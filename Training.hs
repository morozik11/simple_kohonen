{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Training where

import System.Random
import Normalize

class LengV v a where
  lengv ::  v a -> Int 



instance (Ord a, Num a, Fractional a) => LengV (Vectors v) a where
  lengv (Vectors []) = 0
  lengv (Vectors vs) = length vs

instance (Ord a, Num a, Fractional a) => LengV Vector a where
  lengv (Vector []) = 0
  lengv (Vector v) = length v



------------------------------------------------


getRandom :: IO Float
getRandom = randomRIO (0.1,0.3  :: Float)

getLengthVector :: (Num a, Ord a, Fractional a) =>  (Vectors v) a -> Int
getLengthVector (Vectors []) = 0
getLengthVector (Vectors vs) =
  let vector = head vs
  in
    lengv vector


createSampleVector :: (Ord a, Num a) => a -> [a] -> Vector a
createSampleVector leng
  | leng < 1 = do
      return Vector []
  | leng > 0 = do
      return Vector []








