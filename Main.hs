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

multi :: Int -> K [Board]
multi np = do
    [shuffled] <- grabble fullDeck 1 (np * 13) 
    let hand13s = zip [0..] $ splice shuffled (replicate np 13)
    forM_ hand13s $ \(n, hand13) -> do
        s <- get
        let hand = take 5 hand13
        chooseFirstFive n hand
    forM_ [5..12]$ \m -> do
        forM_ hand13s $ \(n, hand13) -> do
            let card = hand13 !! m
            s <- get
            chooseOne n card
    s <- get
    return (view boards $ s)

main :: IO ()
main = do
    [sn] <- getArgs
    let n = read sn :: Int

    brds <- evalStateT (multi n) (initialState n)
    mapM_ printBoard brds
    let score = scoreGame $ map (fillBoard []) brds
    print score
