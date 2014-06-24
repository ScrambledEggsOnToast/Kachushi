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
import Data.List (maximumBy, sortBy, (\\))
import Data.Function (on)
import Data.Array
import qualified Data.Vector as V

---------------------------
--  Constructors
---------------------------

randomFill :: (MonadRandom m) => [Card] -> Board -> m FilledBoard
randomFill deck board = do
    let cardsLeft = 27 - nextTop board
                       - nextMiddle board 
                       - nextBottom board
    [newCards] <- grabble deck 1 cardsLeft

    let oldRows = map (map (\(Filled c) -> c) 
                . takeWhile (/= Empty)) 
                . splice (elems . asArray $ board) 
                $ [3,5,5]

        additions = splice newCards 
            [4 - nextTop board, 9 - nextMiddle board, 14 - nextBottom board]

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
rowsToBoard rs h = toBoard t m b where
    cs = zip rs h
    t = map snd . filter ((== Top) . fst) $ cs
    m = map snd . filter ((== Middle) . fst) $ cs
    b = map snd . filter ((== Bottom) . fst) $ cs

chooseFirstFive :: MonadRandom m => [Card] -> m [Row]
chooseFirstFive hand = do
    let d = fullDeck \\ hand
        rsbs = parMap rdeepseq (\rs -> (rs, rowsToBoard rs hand)) initialChoices

    xs <- forM rsbs $ \(rs,b) -> liftM (rs,) $ replicateM 10 $ randomFill d b

    let scoredRows = parMap rdeepseq (second (sum . map score)) xs
        best = maximumBy (compare `on` snd) scoredRows

    return $ fst best

rowsToCheck st
    | 4 == (nextTop . view board $ st) = [Middle, Bottom]
    | 9 == (nextMiddle . view board $ st) = [Top, Bottom]
    | 14 == (nextBottom . view board $ st) = [Top, Middle]
    | otherwise = [Top, Middle, Bottom]

rowToBoard r s c = view board $ execState (putCard c r) s

chooseOne :: MonadRandom m => Card -> KState -> m Row
chooseOne _ (KState (Board _ _ 9 14) _) = return Top
chooseOne _ (KState (Board _ 4 _ 14) _) = return Middle
chooseOne _ (KState (Board _ 4 9 _) _) = return Bottom
chooseOne card s = do
    let d = view deck s \\ [card]
        rbs = map (\r -> (r, rowToBoard r s card)) (rowsToCheck s)

    xs <- forM rbs $ \(r,b) -> liftM (r,) $ replicateM 100 $ randomFill d b

    let scoredRows = map (second (sum . parMap rdeepseq score)) xs
        best = maximumBy (compare `on` snd) scoredRows

    return $ fst best

splice :: [a] -> [Int] -> [[a]]
splice [] [] = []
splice xs [] = [xs]
splice xs (n:ns) = take n xs : splice (drop n xs) ns
