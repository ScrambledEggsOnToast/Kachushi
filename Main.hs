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

mainK :: K ()
mainK = do
    [hand13] <- grabble fullDeck 1 13 
    let hand = take 5 hand13
    rs <- chooseFirstFive hand
    putCards (zip hand rs)
    forM_ [5..12] $ \n -> do
        let c = hand13 !! n
        s <- get
        r <- chooseOne c s
        putCard c r
    s <- get
    let sc = score . fillBoard [] . view board $ s
    displayBoard
    liftIO $ print sc

main :: IO ()
main = do
    evalStateT mainK initialState