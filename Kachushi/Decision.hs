{-# LANGUAGE TupleSections #-}
module Kachushi.Decision where

import Grabble.Grabble

import Kachushi.Cards
import Kachushi.HandAnalyse
import Kachushi.OFCP
import Kachushi.KState

import Control.Parallel.Strategies
import Control.Monad.Random
import Control.Monad
import Control.Monad.State
import Control.Lens
import Control.Arrow
import Data.List (sortBy, (\\))
import Data.Function (on)
import Data.Array

import Control.Parallel.Strategies

---------------------------
--  Constructors
---------------------------

randomFill :: (MonadRandom m) => [Card] -> Board -> m FilledBoard
randomFill deck board = do
    let cardsLeft = 27 - (nextTop board)
                       - (nextMiddle board) 
                       - (nextBottom board)
    [newCards] <- grabble deck 1 cardsLeft

    let oldRows = map (map (\(Filled c) -> c) 
                . takeWhile (/= Empty)) 
                . splice (elems . asArray $ board) 
                $ [3,5,5]

        additions = splice newCards 
            [4-(nextTop board), 9-(nextMiddle board), 14-(nextBottom board)]

        [t, m, b] = zipWith (++) oldRows additions

    return (FilledBoard t m b)

initialChoices :: [[Row]]
initialChoices = go [[]] 3 5
    where
        go xs _ 0 = xs
        go xs t n = (if t == 0 then [] 
                        else go (map (Top:) xs) (t-1) (n-1))
                          ++ go (map (Middle:) xs) t (n-1)
                          ++ go (map (Bottom:) xs) t (n-1)

---------------------------
--  Choosers
---------------------------

chooseFirstFive :: MonadRandom m => [Card] -> m [Row]
chooseFirstFive hand = do
    let d = fullDeck \\ hand
    let rowsToBoard rs = toBoard t m b where
            cs = zip rs hand
            t = map snd . filter ((== Top) . fst) $ cs
            m = map snd . filter ((== Middle) . fst) $ cs
            b = map snd . filter ((== Bottom) . fst) $ cs
    let rsbs = parMap rdeepseq (\rs -> (rs, rowsToBoard rs)) initialChoices
    xs <- forM rsbs $ \(rs,b) -> liftM (rs,) $ replicateM 10 $ randomFill d b
    let scoredRows = parMap rdeepseq (second (sum . map score)) xs
    let best = last . sortBy (compare `on` snd) $ scoredRows
    return $ fst best

chooseOne :: MonadRandom m => Card -> KState -> m Row
chooseOne card s = do
    let t = 4 - (nextTop . view board $ s)
        m = 9 - (nextMiddle . view board $ s)
        b = 14 - (nextBottom . view board $ s)
        rs = (if t == 0 then [] else [Top])
          ++ (if m == 0 then [] else [Middle])
          ++ (if b == 0 then [] else [Bottom])
    let d = (view deck s) \\ [card]
    let rowToBoard r = view board $ execState (putCard card r) s
    let rbs = map (\r -> (r, rowToBoard r)) rs
    xs <- forM rbs $ \(r,b) -> liftM (r,) $ replicateM 100 $ randomFill d b
    let scoredRows = map (second (sum . parMap rdeepseq score)) xs
    let best = last . sortBy (compare `on` snd) $ scoredRows
    return $ fst best

splice :: [a] -> [Int] -> [[a]]
splice [] [] = []
splice xs [] = [xs]
splice xs (n:ns) = (take n xs):(splice (drop n xs) ns)
