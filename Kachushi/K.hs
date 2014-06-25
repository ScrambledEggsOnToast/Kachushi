{-# LANGUAGE TypeSynonymInstances, FlexibleInstances#-}
module Kachushi.K where

import Control.Monad.Random
import Control.Monad.State
import Control.Lens

import Kachushi.KState
import Kachushi.OFCP

type K = StateT KState IO

displayBoard :: Int -> K ()
displayBoard n = do s <- get; liftIO $ printBoard . (!! n) . view boards $ s

displayBoards :: Int -> K ()
displayBoards pp = do s <- get; liftIO $ printBoards pp . view boards $ s

