{-# LANGUAGE TupleSections, FlexibleContexts #-}
module Kachushi.Decision 
(
    chooseFirstFive
  , chooseOne
)  where

import Grabble.Grabble

import Kachushi.Cards (Card (..))
import Kachushi.OFCP (Board (..), FilledBoard (..), Row (..), Slot (..), toBoard, scoreGame)
import Kachushi.KState (KState (..), boards, deck, putCard, putCards)
import Kachushi.Util (splice)

import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq (force)
import Control.Monad.Random (MonadRandom (..))
import Control.Monad (liftM, replicateM)
import qualified Control.Monad.Parallel as MP (mapM, MonadParallel (..))
import Control.Monad.State (MonadState (..), StateT (..), runStateT, evalStateT, execState, modify)
import Control.Lens (view, (&), ix, (.~), set)
import Control.Arrow (second)
import Data.List (maximumBy, sortBy, (\\))
import Data.Function (on)
import Data.Array (elems)

instance MP.MonadParallel m => MP.MonadParallel (StateT s m) where
    bindM2 f ma mb = 
        StateT (\s -> let f' a b = runStateT (f a b) s 
            in MP.bindM2 f' (evalStateT ma s) (evalStateT mb s))            

---------------------------
--  Constructor
---------------------------

randomFills :: (MonadRandom m) => [Card] -> [Board] -> m [FilledBoard]
randomFills deck boards = do
    let cardsLefts = map (\board -> 27 - nextTop board
                                       - nextMiddle board 
                                       - nextBottom board) $ boards
    [newCards] <- liftM force $ grabble deck 1 (sum cardsLefts)
    let newCardss = splice newCards cardsLefts

    let oldRowss = map (\board -> map (map (\(Filled c) -> c) 
                                . takeWhile (/= Empty)) 
                                . splice (elems . asArray $ board) 
                                $ [3,5,5]) boards
        
        additionCounts = (flip map) boards (\board -> 
            [4 - nextTop board, 9 - nextMiddle board, 14 - nextBottom board])

        additionss = zipWith splice newCardss additionCounts

        tmbs = zipWith (zipWith (++)) oldRowss additionss

    return $ force $ map (\[t, m, b] -> FilledBoard t m b) tmbs


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

chooseFirstFive :: (MonadState KState m, MonadRandom m, MP.MonadParallel m) 
    => Int -> [Card] -> m ()
chooseFirstFive n hand = do
    s <- get
    let d = (view deck s) \\ hand
        sb = view boards s

    xs <- (flip MP.mapM) initialChoices $ \rs -> do 
        let b = force $ rowsToBoard rs hand
        liftM (b,) $ replicateM 10 $ randomFills d (sb & ix n .~ b)
    
    let scoredRows = parMap rdeepseq 
                        (second (sum . map ((!! n) . scoreGame))) xs
        best = maximumBy (compare `on` snd) scoredRows
    
    modify (set boards (sb & ix n .~ (fst best)) . set deck d)

rowsToCheck n st
    | 4 == (nextTop . (!! n) . view boards $ st) = [Middle, Bottom]
    | 9 == (nextMiddle . (!! n) . view boards $ st) = [Top, Bottom]
    | 14 == (nextBottom . (!! n) . view boards $ st) = [Top, Middle]
    | otherwise = [Top, Middle, Bottom]

rowToBoard n r s c = (!! n) . view boards $ execState (putCard n c r) s

chooseOne :: (MonadState KState m, MonadRandom m, MP.MonadParallel m) 
    => Int -> Card -> m ()
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

            xs <- MP.mapM (\(r,b) -> liftM (b,) $ replicateM 500
                $ randomFills d (sb & ix n .~ b)) rbs

            let scoredRows = map 
                    (second (sum . parMap rdeepseq ((!! n) . scoreGame))) xs
                best = maximumBy (compare `on` snd) scoredRows
            
            modify (set boards (sb & ix n .~ (fst best)) . set deck d)

