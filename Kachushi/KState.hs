{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Kachushi.KState 
(
    KState (..)
  , boards
  , deck
  , initialState
  , putCard
  , putCards
) where

import Kachushi.Cards (Card (..), fullDeck)
import Kachushi.OFCP (Board (..), emptyBoard, Slot (..), Row (..))

import Control.Lens (over, makeLenses, element)
import Control.Monad.State (MonadState (..), get, modify, forM_)
import Data.List ((\\))
import Data.Array.ST (runSTArray, thaw, newListArray, STArray (..), readArray, writeArray)
import Control.Monad.ST (ST (..))
import Control.Applicative ((<*>))

---------------------------
--  Types
---------------------------

data KState = KState { _boards :: [Board] , _deck :: [Card] } deriving Show
makeLenses ''KState

---------------------------
--  Costructors
---------------------------

initialState n = KState (replicate n emptyBoard) fullDeck

---------------------------
--  State monad
---------------------------

putCard :: MonadState KState m => Int -> Card -> Row -> m ()
putCard n card row = putCards n [(card, row)]

putCards :: MonadState KState m => Int -> [(Card, Row)] -> m ()
putCards n crs = let
        boardArray brd = runSTArray $ do
            array <- thaw (asArray brd)
            empties <- newListArray (0,2) 
                $ [nextTop, nextMiddle, nextBottom] <*> [brd]
                    :: (ST s) ((STArray s) Int Int)
            forM_ crs $ \(card, row) -> do
                index <- readArray empties (fromEnum row)
                writeArray empties (fromEnum row) (index + 1)
                writeArray array index (Filled card)
            return array
        t brd = (nextTop brd) 
            + (length . filter (== Top) . map snd $ crs)
        m brd = (nextMiddle brd) 
            + (length . filter (== Middle) . map snd $ crs)
        b brd = (nextBottom brd) 
            + (length . filter (== Bottom) . map snd $ crs)
    in
        do
            state <- get
            modify $ over deck (\\ (map fst crs) )
                . over boards (over (element n) 
                    (\brd -> (Board (boardArray brd) (t brd) (m brd) (b brd))))
