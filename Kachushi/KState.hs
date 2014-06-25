{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Kachushi.KState where

import Kachushi.Cards
import Kachushi.OFCP

import Control.Lens
import Control.Monad.State
import Data.List ((\\))
import Data.Array.ST
import Control.Monad.ST
import Control.Applicative

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
putCard n card row = 
    modify $ over deck (\\ [card]) 
           . over boards (over (element n) (putInBoard card row))
    where
        putInBoard card row board = let
                t = if row == Top 
                        then nextTop board + 1 
                        else nextTop board
                m = if row == Middle 
                        then nextMiddle board + 1 
                        else nextMiddle board
                b = if row == Bottom 
                        then nextBottom board + 1 
                        else nextBottom board
                arr = runSTArray $ do
                    array <- thaw (asArray board)
                    let index = ([nextTop, nextMiddle, nextBottom] <*> [board]) 
                                !! fromEnum row
                    writeArray array index (Filled card)
                    return array
            in
                Board arr t m b

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
