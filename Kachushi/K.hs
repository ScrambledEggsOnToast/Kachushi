{-# LANGUAGE TypeSynonymInstances, FlexibleInstances#-}
module Kachushi.K 
(
    K (..)
  , displayBoards
) where

import Control.Monad.State (StateT, get, liftIO)
import Control.Lens (view)

import Kachushi.KState (KState (..), boards)
import Kachushi.OFCP (printBoard, printBoards)

type K = StateT KState IO

displayBoard :: Int -> K ()
displayBoard n = do s <- get; liftIO $ printBoard . (!! n) . view boards $ s

displayBoards :: Int -> K ()
displayBoards pp = do s <- get; liftIO $ printBoards pp . view boards $ s

