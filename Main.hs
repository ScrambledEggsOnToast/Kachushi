module Main where

import Grabble.Grabble

import Kachushi.Cards
import Kachushi.K
import Kachushi.Decision
import Kachushi.KState
import Kachushi.OFCP

import Control.Lens
import Control.Monad
import Control.Monad.State
import System.Environment (getArgs)
import System.IO
import Data.Time.Clock
import Data.List (transpose)
import Data.Maybe
import qualified Data.Set as Set

time :: IO DiffTime
time = getCurrentTime >>= return . utctDayTime

printT :: (Show a) => DiffTime -> a -> IO ()
printT rt x = do
    t <- time
    let dt = t - rt
    putStrLn $ show dt ++ ": " ++ show x

multi :: Int -> K [Board]
multi np = do
    t <- liftIO time
    
    let pr = liftIO . printT t

    pr "begin"

    [shuffled] <- grabble (Set.toList fullDeck) 1 (np * 13) 
    let hand13s = zip [0..] $ splice shuffled (replicate np 13)

    pr "generated hands"

    forM_ hand13s $ \(n, hand13) -> do
        let hand = take 5 hand13
        chooseFirstFive n hand
        pr $ "player " ++ show n ++ " placed first five"
    forM_ [5..12]$ \m -> do
        forM_ hand13s $ \(n, hand13) -> do
            let card = hand13 !! m
            chooseOne n card
            pr $ "player " ++ show n ++ " placed card " ++ show (m + 1)
    s <- get

    return (view boards $ s)

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

pvc :: Int -> K [Board]
pvc np = do
    [shuffled] <- grabble (Set.toList fullDeck) 1 (np * 13)
    let hand13s = splice shuffled (replicate np 13)
    let hand13p = head hand13s
    let hand13sc = zip [1..] $ tail hand13s

    let handp = take 5 hand13p
    displayBoards
    liftIO $ print handp

    forM_ handp $ \c -> do
        liftIO $ putStr $ show c ++ ": "
        r <- checkedGetRow 0
        liftIO $ putStrLn $ show r
        putCard 0 c r

    forM_ hand13sc $ \(n, hand13) -> do
        let hand = take 5 hand13
        chooseFirstFive n hand
    forM_ [5..12]$ \m -> do
        displayBoards
        let cardp = hand13p !! m
        liftIO $ putStr $ show cardp ++ ": "
        choice <- checkedGetRow 0
        liftIO $ putStrLn $ show choice
        putCard 0 cardp choice
        forM_ hand13sc $ \(n, hand13) -> do
            let card = hand13 !! m
            chooseOne n card
    displayBoards
    s <- get
    return (view boards $ s)
    

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    
    [sn] <- getArgs
    let n = read sn :: Int

    brds <- evalStateT (pvc n) (initialState n)
    
    print . scoreGame . map (fillBoard []) $ brds
