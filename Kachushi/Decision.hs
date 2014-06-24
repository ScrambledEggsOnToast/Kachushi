{-# LANGUAGE TupleSections, FlexibleContexts #-}
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

randomFills :: MonadRandom m => [Card] -> [Board] -> m [FilledBoard]
randomFills deck boards = do
    let cardsLefts = map (\board -> 27 - nextTop board
                                       - nextMiddle board 
                                       - nextBottom board)
                   $ boards
    [newCards] <- grabble deck 1 (sum cardsLefts)
    let newCardss = splice newCards cardsLefts

    let oldRowss = map (\board -> map (map (\(Filled c) -> c) 
                                . takeWhile (/= Empty)) 
                                . splice (elems . asArray $ board) 
                                $ [3,5,5]) boards
        
        additionCounts = map (\board -> 
            [4 - nextTop board, 9 - nextMiddle board, 14 - nextBottom board]) boards
        additionss = zipWith splice newCardss additionCounts

        tmbs = zipWith (zipWith (++)) oldRowss additionss

    return $ map (\[t, m, b] -> FilledBoard t m b) tmbs


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

chooseFirstFive :: (MonadState KState m, MonadRandom m) => Int -> [Card] -> m ()
chooseFirstFive n hand = do
    s <- get
    let d = (view deck s) \\ hand
        rsbs = parMap rdeepseq (\rs -> (rs, rowsToBoard rs hand)) initialChoices
        sb = view boards s
    xs <- forM rsbs $ \(rs,b) -> liftM (b,) $ replicateM 10 $ randomFills d (sb & ix n .~ b)

    let scoredRows = parMap rdeepseq (second (sum . map ((!! n) . scoreGame))) xs
        best = maximumBy (compare `on` snd) scoredRows
    
    modify (set boards (sb & ix n .~ (fst best)) . set deck d)

rowsToCheck n st
    | 4 == (nextTop . (!! n) . view boards $ st) = [Middle, Bottom]
    | 9 == (nextMiddle . (!! n) . view boards $ st) = [Top, Bottom]
    | 14 == (nextBottom . (!! n) . view boards $ st) = [Top, Middle]
    | otherwise = [Top, Middle, Bottom]

rowToBoard n r s c = (!! n) . view boards $ execState (putCard n c r) s

chooseOne :: (MonadState KState m, MonadRandom m) => Int -> Card -> m ()
chooseOne n card = do
    s <- get
    case (!! n) . view boards $ s of
        Board _ _ 9 14 -> putCard n card Top
        Board _ 4 _ 14 -> putCard n card Middle
        Board _ 4 9 _ -> putCard n card Bottom
        otherwise -> do
            let d = view deck s \\ [card]
                rbs = map (\r -> (r, rowToBoard n r s card)) (rowsToCheck n s)
                sb = view boards s

            xs <- forM rbs $ \(r,b) -> liftM (b,) $ replicateM 100 $ randomFills d (sb & ix n .~ b)

            let scoredRows = map (second (sum . parMap rdeepseq ((!! n) . scoreGame))) xs
                best = maximumBy (compare `on` snd) scoredRows
            
            modify (set boards (sb & ix n .~ (fst best)) . set deck d)

splice :: [a] -> [Int] -> [[a]]
splice [] [] = []
splice xs [] = [xs]
splice xs (n:ns) = take n xs : splice (drop n xs) ns
