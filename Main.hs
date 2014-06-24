module Main where

import Grabble.Grabble

import Kachushi.Cards
import Kachushi.K
import Kachushi.Decision
import Kachushi.KState
import Kachushi.OFCP

import Control.Lens
import Control.Monad
import Control.Monad.State
import System.Environment (getArgs)

mainK :: K Int
mainK = do
    [hand13] <- grabble fullDeck 1 13 
    let hand = take 5 hand13
    rs <- chooseFirstFive hand
    putCards (zip hand rs)
    forM_ [5..12] $ \n -> do
        displayBoard
        let c = hand13 !! n
        liftIO $ print c
        s <- get
        r <- chooseOne c s
        putCard c r
    s <- get
    let sc = score . fillBoard [] . view board $ s
    displayBoard
    return sc

main :: IO ()
main = do
    [sn] <- getArgs
    let n = read sn
    scs <- replicateM n $ evalStateT mainK initialState
    print ((/ (fromInteger . toInteger $ n)) . fromInteger . toInteger . sum $ scs)
