module Main where

import Grabble.Grabble

import Kachushi.Cards (Card, colorPutCard, fullDeck)
import Kachushi.K (K (..), displayBoards)
import Kachushi.Decision (chooseOne, chooseFirstFive)
import Kachushi.KState (putCard, putCards, boards, initialState)
import Kachushi.OFCP (Board (..), Row (..), scoreGame, fillBoard)
import Kachushi.Util (splice)

import Control.Lens (view)
import Control.Monad (forM_)
import Control.Monad.State (liftIO, get, evalStateT)
import System.Environment (getArgs)
import Data.List (intersperse)
import System.IO (stdout, stdin, hSetEcho, hSetBuffering, BufferMode (..))
import Data.Maybe (fromMaybe, fromJust)
import Text.Read (readMaybe)

getRow :: IO (Maybe Row)
getRow = do
    hSetEcho stdout False
    c <- getChar
    hSetEcho stdout True
    if c `elem` "qwertyuiop" then return (Just Top)
        else if c `elem` "asdfghjkl" then return (Just Middle)
            else if c `elem` "zxcvbnm" then return (Just Bottom)
                else return Nothing

defGetRow :: IO Row
defGetRow = do
    r <- getRow
    if r == Nothing then defGetRow else return (fromJust r)

checkedGetRow :: Int -> K Row
checkedGetRow n = do
    r <- liftIO defGetRow
    s <- get
    let brd = (!! n) . view boards $ s
        t = nextTop brd
        m = nextMiddle brd
        b = nextBottom brd
    case r of
        Top -> if t == 4 then checkedGetRow n else return Top
        Middle -> if m == 9 then checkedGetRow n else return Middle
        Bottom -> if b == 14 then checkedGetRow n else return Bottom

-- Present a card and request a row from the player, ensuring that there
-- is space in the row of the nth board for that card
requestRow :: Int -> Card -> K Row 
requestRow n c = do
    liftIO $ colorPutCard c
    liftIO $ putStr ": "
    r <- checkedGetRow n
    liftIO $ putStrLn $ show r
    return r

divider :: IO ()
divider = putStrLn "\n--------\n"

printHand :: [Card] -> IO ()
printHand hand = do
    sequence_ . intersperse (putStr ", ") . map colorPutCard $ hand
    putStrLn ""

pvc :: Int -> Int -> K ()
pvc np pp = do
    [shuffled] <- grabble fullDeck 1 (np * 13)
    let hand13s = zip [0..] $ splice shuffled (replicate np 13)

    forM_ hand13s $ \(n, hand13) -> do
        let hand = take 5 hand13
        if n == pp then do
            displayBoards pp
            liftIO $ printHand hand

            forM_ hand $ \c -> do
                r <- requestRow pp c
                putCard pp c r
            else do
                chooseFirstFive n hand

    forM_ [5..12]$ \m -> do
        forM_ hand13s $ \(n, hand13) -> do
            let c = hand13 !! m
            if n == pp then do
                    liftIO divider
                    displayBoards pp
                    r <- requestRow pp c
                    putCard pp c r
                else do
                    chooseOne n c
    liftIO divider
    displayBoards pp

    s <- get
    liftIO $ do 
        putStrLn ""
        putStrLn "Final Score:"
        print . scoreGame . map (fillBoard []) . view boards $ s

    return ()
    
usage :: IO ()
usage = putStrLn 
    "Usage: Kachushi n p\n\
    \  n = Number of players (2, 3, or 4).\n\
    \  p = Human player's seat (0, 1, 2, or 3). A number out of this range\n\
    \  will cause all players to be computer-controlled.\n\
    \  To place cards in: the top row, press one of q,w,e,r,t,y,u,i,o,p,\n\
    \                     the middle row, press one of a,s,d,f,g,h,j,k,l\n\
    \                     the bottom row, press one of z,x,c,v,b,n,m"

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    
    args <- getArgs

    if "--h" `elem` args then usage else do
        let argsIntMaybe = mapM readMaybe args :: Maybe [Int]
        fromMaybe usage $ do
            argsInt <- argsIntMaybe
            if length argsInt /=2 then Nothing else do
                let [n,p] = argsInt
                if n `elem` [2,3,4] 
                    then return $ evalStateT (pvc n p) (initialState n)
                    else Nothing

