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
import Data.List (transpose, intersperse)
import Data.Maybe

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

    [shuffled] <- grabble fullDeck 1 (np * 13) 
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


pvc :: Int -> Int -> K [Board]
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
    return (view boards $ s)
    

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    
    [n,p] <- liftM (map read) getArgs :: IO [Int]

    brds <- evalStateT (pvc n p) (initialState n)
    
    print . scoreGame . map (fillBoard []) $ brds
