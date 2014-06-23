{-# LANGUAGE FlexibleContexts #-}
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

data KState = KState { _board :: Board , _deck :: [Card] } deriving Show

board :: Lens' KState Board
board _f_a2Eo7 (KState __board'_a2Eo8 __deck_a2Eoa)
  = ((\ __board_a2Eo9 -> KState __board_a2Eo9 __deck_a2Eoa)
     <$> (_f_a2Eo7 __board'_a2Eo8))
{-# INLINE board #-}
deck :: Lens' KState [Card]
deck _f_a2Eob (KState __board_a2Eoc __deck'_a2Eod)
  = ((\ __deck_a2Eoe -> KState __board_a2Eoc __deck_a2Eoe)
     <$> (_f_a2Eob __deck'_a2Eod))
{-# INLINE deck #-}

---------------------------
--  Costructors
---------------------------

initialState = KState emptyBoard fullDeck

---------------------------
--  State monad
---------------------------

putCard :: MonadState KState m => Card -> Row -> m ()
putCard card row = modify $ over deck (\\ [card]) 
                          . over board (putInBoard card row) 
    where
        putInBoard card row board = let
                t = if row == Top 
                        then (nextTop board) + 1 
                        else nextTop board
                m = if row == Middle 
                        then (nextMiddle board) + 1 
                        else nextMiddle board
                b = if row == Bottom 
                        then (nextBottom board) + 1 
                        else nextBottom board
                arr = runSTArray $ do
                    array <- thaw (asArray board)
                    let index = ([nextTop, nextMiddle, nextBottom] <*> [board]) 
                                !! (fromEnum row)
                    writeArray array index (Filled card)
                    return array
            in
                Board arr t m b

putCards :: MonadState KState m => [(Card, Row)] -> m ()
putCards crs = let
        boardArray state = runSTArray $ do
            array <- thaw (asArray (view board state))
            empties <- newListArray (0,2) 
                $ [nextTop, nextMiddle, nextBottom] <*> [(view board state)]
                    :: (ST s) ((STArray s) Int Int)
            forM_ crs $ \(card, row) -> do
                index <- readArray empties (fromEnum row)
                writeArray empties (fromEnum row) (index + 1)
                writeArray array index (Filled card)
            return array
        t state = (nextTop . view board $ state) 
            + (length . filter (== Top) . map snd $ crs)
        m state = (nextMiddle . view board $ state) 
            + (length . filter (== Middle) . map snd $ crs)
        b state = (nextBottom . view board $ state) 
            + (length . filter (== Bottom) . map snd $ crs)
    in
        do
            state <- get
            modify $ over deck (\\ (map fst crs)) 
                . over board (const (Board (boardArray state) 
                    (t state) (m state) (b state)))
