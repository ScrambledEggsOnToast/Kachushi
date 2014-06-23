module Kachushi.OFCP where

import Kachushi.Cards
import Kachushi.HandAnalyse

import Data.Array
import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Data.List (sortBy, group, intercalate)
import Data.Function (on)
import Control.Applicative

---------------------------
--  Types
---------------------------

data Row = Top | Middle | Bottom deriving (Show, Enum, Eq)
data Slot = Empty | Filled Card deriving (Show, Eq)
data Board = Board 
    { asArray :: Array Int Slot
    , nextTop :: Int
    , nextMiddle :: Int
    , nextBottom :: Int } deriving Show
data FilledBoard = FilledBoard { top :: [Card], middle :: [Card], bottom :: [Card] } deriving Show

---------------------------
--  Constructors
---------------------------

emptyBoard :: Board
emptyBoard = Board (array (1,13) (zip [1..13] (repeat Empty))) 1 4 9

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
                         . last 
                         . sortBy (compare `on` length) 
                         . group 
                         . map rank 
                            $ hand

compareHand = compare `on` rating

score :: FilledBoard -> Int
score (FilledBoard t m b) = if compareHand t m == GT && compareHand m b == GT 
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
    mapM_ (putStrLn . intercalate " ") [t,m,b]
