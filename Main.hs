module Main where




getMax :: Ord t => [t]  -> t
getMax vector = maximum vector


getMin :: Ord t => [t]  -> t
getMin vector = minimum vector 


wrapGetMin :: Ord t => [[t]] -> t -> t
wrapGetMin [] i = i
wrapGetMin (x:xs) i = 
    let c = getMin x
        in
            if c < i
                then wrapGetMin xs c
                else wrapGetMin xs i


wrapGetMax :: Ord t => [[t]] -> t -> t
wrapGetMax [] i  = i
wrapGetMax (x:xs) i  = 
    let c  = getMax x
        in
            if c > i 
            then wrapGetMax xs c
            else wrapGetMax xs i


getA :: (Ord t, Fractional t) => t -> t -> t
getA 0 0 = 0
getA max min = 
    let delta = max - min
    in
        if delta > 0
            then 1/ delta
            else 0


wrapGetA :: (Ord t, Fractional t) => [[t]] -> t
wrapGetA []  = 0
wrapGetA vectors@(x:xs) =
    getA max min
    where 
        max = wrapGetMax vectors 0
        min = wrapGetMin vectors 0


getB :: (Ord t, Fractional t) => t -> t -> t
getB 0 0 = 0
getB max min = 
    let delta = max - min
    in
        if delta > 0
            then -1* min/delta
            else 0


wrapGetB :: (Ord t, Fractional t) => [[t]] -> t
wrapGetB [] = 0
wrapGetB vectors@(x:xs) =
    getB max min
    where 
        max = wrapGetMax vectors 0
        min = wrapGetMin vectors 0
 

--wrapNormalizData :: (Ord t, Fractional t) => [t] -> t -> t ->  [t] -> [t] 
--wrapNormalizData [] _ _ vectors@(x:xs) = [] : vectors
--wrapNormalizData (x:xs) a b vectors@(z:zs)  =  
--    let n =  a * x + b
--    in n : vectors
--    wrapNormalizData xs a b vectors

normalizData :: (Ord t, Fractional t)  => [t] ->t -> t  -> [t]
normalizData vector a b =  map (\x -> a*x+b ) vector
    

main :: IO()
main = do
    return ()
