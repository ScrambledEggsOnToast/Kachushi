module Grabble.Grabble (grabble) where

import Control.Monad
import Control.Monad.Random
import GHC.Arr (elems)
import Data.Array.ST

grabble :: MonadRandom m => [a] -> Int -> Int -> m [[a]]
grabble xs m n = do
    swapss <- replicateM m $ forM [0 .. min (maxIx - 1) n] $ \i -> do
                j <- getRandomR (i, maxIx)
                return (i, j)
    return $ map (take n . swapElems xs) swapss
    where
        maxIx   = length xs - 1
 
grabbleOnce :: MonadRandom m => [a] -> Int -> m [a]
grabbleOnce xs n = head `liftM` grabble xs 1 n
 
swapElems  :: [a] -> [(Int, Int)] -> [a]
swapElems xs swaps = elems $ runSTArray (do
    arr <- newListArray (0, maxIx) xs
    mapM_ (swap arr) swaps
    return arr)
    where
        maxIx   = length xs - 1
        swap arr (i,j) = do
            vi <- readArray arr i
            vj <- readArray arr j
            writeArray arr i vj
            writeArray arr j vi
