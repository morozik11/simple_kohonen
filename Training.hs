

module Training where

import System.Random
  





--wrapGetRandom :: IO Float -> Float
--wrapGetRandom m = m

test = do
  r <- randomRIO (0.1,0.3  :: Float)
  pure (2 * r)

--getRandom :: Float
getRandom = randomRIO (0.1,0.3  :: Float)

