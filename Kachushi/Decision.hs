module Kachushi.Decision where

import Grabble.Grabble

import Kachushi.Cards
import Kachushi.HandAnalyse
import Kachushi.OFCP
import Kachushi.KState

import Control.Monad.Random
import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.List (sortBy)
import Data.Function (on)
import Data.Array

randomFill :: (MonadRandom m) => [Card] -> Board -> m FilledBoard
randomFill deck board = do
    [newCards] <- grabble deck 1 (27 - (nextTop board) - (nextMiddle board) - (nextBottom board))
    let oldRows = map (map (\(Filled c) -> c) . takeWhile (/= Empty)) $ splice (elems . asArray $ board) [3,5,5]
        additions = splice newCards [4-(nextTop board), 9-(nextMiddle board), 14-(nextBottom board)]
        [t, m, b] = zipWith (++) oldRows additions
    return (FilledBoard t m b)

initialChoices :: [[Row]]
initialChoices = initialChoices [[]] 3 5
    where
        initialChoices xs _ 0 = xs
        initialChoices xs t n = (if t == 0 then [] else initialChoices (map (Top:) xs) (t-1) (n-1))
                                 ++ initialChoices (map (Middle:) xs) t (n-1)
                                 ++ initialChoices (map (Bottom:) xs) t (n-1)


chooseFirstFive :: MonadRandom m => [Card] -> m [Row]
chooseFirstFive hand = do
    let rss = initialChoices
    xs <- forM rss $ \rs -> replicateM 10 (do
        let state = execState (putCards (zip hand rs)) initialState
        randomFill (view deck state) (view board state))
    let scores = map (sum . map score) $ xs
    let best = last . sortBy (compare `on` snd) $ zip rss scores
    return $ fst best

chooseOne :: MonadRandom m => Card -> KState -> m Row
chooseOne card s = do
    let t = 4 - (nextTop . view board $ s)
        m = 9 - (nextMiddle . view board $ s)
        b = 14 - (nextBottom . view board $ s)
        rs = (if t == 0 then [] else [Top])
          ++ (if m == 0 then [] else [Middle])
          ++ (if b == 0 then [] else [Bottom])
    xs <- forM rs $ \r -> (do
        let state = execState (putCard card r) s
        results <- replicateM 100 (randomFill (view deck state) (view board state))
        return (r, (sum . map score $ results)))
    let best = last . sortBy (compare `on` snd) $ xs
    return $ fst best

splice :: [a] -> [Int] -> [[a]]
splice [] [] = []
splice xs [] = [xs]
splice xs (n:ns) = (take n xs):(splice (drop n xs) ns)
