{-# LANGUAGE TupleSections #-}

module Kachushi.OFCP where

import Kachushi.Cards
import Kachushi.HandAnalyse

import Data.Array
import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Data.List (maximumBy, sortBy, group, intercalate)
import Data.Function (on)
import Control.Applicative
import Control.DeepSeq

---------------------------
--  Types
---------------------------

data Row = Top | Middle | Bottom deriving (Show, Enum, Eq)
instance NFData Row where
data Slot = Empty | Filled Card deriving (Show, Eq)
data Board = Board 
    { asArray :: Array Int Slot
    , nextTop :: Int
    , nextMiddle :: Int
    , nextBottom :: Int } deriving Show
instance NFData Board where
    rnf (Board arr t m b) = arr `seq` t `seq` m `seq` b `seq` ()
data FilledBoard = FilledBoard { top :: [Card], middle :: [Card], bottom :: [Card] } deriving Show

---------------------------
--  Constructors
---------------------------

emptyBoard :: Board
emptyBoard = Board (array (1,13) (zip [1..13] (repeat Empty))) 1 4 9

toBoard :: [Card] -> [Card] -> [Card] -> Board
toBoard t m b = Board arr nt nm nb
    where
        lt = take 3 $ map Filled t ++ repeat Empty
        lm = take 5 $ map Filled m ++ repeat Empty
        lb = take 5 $ map Filled b ++ repeat Empty
        arr = array (1,13) (zip [1..13] (lt ++ lm ++ lb))
        nt = 1 + length t
        nm = 4 + length m
        nb = 9 + length b

filledBoard :: [Card] -> FilledBoard
filledBoard cards = FilledBoard t m b 
    where
        x = drop 3 cards
        y = drop 5 x
        t = take 3 cards
        m = take 5 x
        b = take 5 y

fillBoard :: [(Card, Row)] -> Board -> FilledBoard
fillBoard choices board = filledBoard . map (\(Filled c) -> c) . elems $ runSTArray (do
    array <- thaw (asArray board)
    empties <- newListArray (0,2) $ [nextTop, nextMiddle, nextBottom] <*> [board] :: (ST s) ((STArray s) Int Int)
    forM_ choices $ \(card, row) -> do
        index <- readArray empties (fromEnum row)
        writeArray empties (fromEnum row) (index + 1)
        writeArray array index (Filled card)
    return array)

---------------------------
--  Board scorer
---------------------------

royalty :: Row -> [Card] -> Int
royalty rowType hand
    | rowType == Bottom = case handType hand of
        Straight -> 2
        Flush -> 4
        FullHouse -> 6
        FourOfAKind -> 10
        StraightFlush -> 15
        RoyalFlush -> 25
        otherwise -> 0
    | rowType == Middle = case handType hand of
        ThreeOfAKind -> 2
        Straight -> 4
        Flush -> 8
        FullHouse -> 12
        FourOfAKind -> 20
        StraightFlush -> 30
        RoyalFlush -> 50
        otherwise -> 0
    | rowType == Top = case handType hand of
        ThreeOfAKind -> 10 + fromEnum relevantRank
        OnePair -> max (fromEnum relevantRank - 3) 0
        otherwise -> 0
        where
            relevantRank = head 
                         . maximumBy (compare `on` length) 
                         . group 
                         . map rank 
                            $ hand

compareHand = flip (compare `on` rating)

matches :: [FilledBoard] -> [(Int, Int, FilledBoard, FilledBoard)]
matches bs = matches' (length bs) where
    matches' 1 = []
    matches' n = map (\i -> (i, n-1, bs !! i, bs !! (n-1))) [0 .. n-2] ++ matches' (n-1)

iisToLs :: Int -> (Int, Int, Int) -> [Int]
iisToLs n (a,b,s) = map (\n -> if n == a then s else if n == b then (-s) else 0) [0 .. n-1]

scoreGame :: [FilledBoard] -> [Int]
scoreGame bs = foldl (zipWith (+)) (repeat 0) . map (iisToLs (length bs)) $ matchScores
    where
        matchScores = map (\(a,b,c,d) -> (a,b,scoreMatch c d)) $ matches bs

scoreMatch :: FilledBoard -> FilledBoard -> Int
scoreMatch brd1@(FilledBoard t1 m1 b1) brd2@(FilledBoard t2 m2 b2)
    | fouled brd1 && fouled brd2 = 0
    | fouled brd2 = 6 + royalty Top t1 + royalty Middle m1 + royalty Bottom b1
    | fouled brd1 = 0 - scoreMatch brd2 brd1
    | otherwise = 
        sum
        [
            wt, wm, wb
          , if abs (wt + wm + wb) == 3 then wt + wm + wb else 0
          , royalty Top t1, royalty Middle m1, royalty Bottom b1
          , 0 - royalty Top t2, 0 - royalty Middle m2, 0 - royalty Bottom b2
        ] where
            wt = fromEnum (compareHand t1 t2) - 1
            wm = fromEnum (compareHand m1 m2) - 1
            wb = fromEnum (compareHand b1 b2) - 1

fouled :: FilledBoard -> Bool
fouled (FilledBoard t m b) = compareHand t m == GT || compareHand m b == GT

score :: FilledBoard -> Int
score (FilledBoard t m b) = if compareHand t m == LT && compareHand m b == LT 
                        then 6 + sum [royalty Top t, royalty Middle m, royalty Bottom b]
                        else (-6)

---------------------------
--  Board printer
---------------------------

printBoard :: Board -> IO ()
printBoard (Board arr _ _ _) = do
    let cs = elems arr
        shown = map showSlot cs
        showSlot Empty = "[]"
        showSlot (Filled c) = show c
        x = drop 3 shown
        y = drop 5 x
        t = "  " : take 3 shown
        m = take 5 x
        b = take 5 y
    mapM_ (putStrLn . unwords) [t,m,b]
