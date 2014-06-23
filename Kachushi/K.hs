{-# LANGUAGE TypeSynonymInstances, FlexibleInstances#-}
module Kachushi.K where

import Control.Monad.Random
import Control.Monad.State
import Control.Lens

import Kachushi.KState
import Kachushi.OFCP

type K = StateT KState IO

instance MonadRandom K where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

displayBoard :: K ()
displayBoard = do s <- get; liftIO $ printBoard . view board $ s
