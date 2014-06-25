module Kachushi.HandAnalyse where

import Kachushi.Cards
import Kachushi.HandAnalyse.Data

import Data.Word
import Data.Bits
import qualified Data.StaticHash as SH
import Data.Maybe
import Data.List ((\\))
import Control.Applicative

---------------------------
--  Types
---------------------------

data HandType = 
    HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind 
  | Straight 
  | Flush 
  | FullHouse  
  | FourOfAKind 
  | StraightFlush  
  | RoyalFlush 
  deriving (Eq, Ord, Show, Read)

---------------------------
--  Functions to deduce type of lookup
---------------------------

flush :: [Card] -> Bool
flush cs = ss `elem` flushes
    where
        ss = map suit cs
        flushes = 
            [
                [C,C,C,C,C]
              , [D,D,D,D,D]
              , [H,H,H,H,H]
              , [S,S,S,S,S]
            ]

unique :: [Card] -> Word16
unique = fromIntegral . (`shiftR` 16) . foldl (.|.) 0 . map binary

nonUnique :: [Card] -> Word32
nonUnique = product . map ((.&. 0xff) . binary)

---------------------------
--  Hand value functions
---------------------------

handValue :: [Card] -> Int
handValue h = if flush h 
                then fromJust . SH.lookup (unique h) $ flushesHash
                else fromMaybe 
                        (fromJust . SH.lookup (nonUnique h) $ nonUniqueHash)
                        (SH.lookup (unique h) uniqueHash)

handValueThree :: [Card] -> Int
handValueThree h = fromJust $ SH.lookup (nonUnique h) threeHash

---------------------------
--  Hand rating function
---------------------------

rating :: [Card] -> Int
rating hand
    | length hand == 5 = handValue hand
    | length hand == 3 = handValueThree hand

---------------------------
--  Hand type function
---------------------------

handType :: [Card] -> HandType
handType hand
    | rating hand <= 1 = RoyalFlush
    | rating hand <= 10 = StraightFlush
    | rating hand <= 166 = FourOfAKind
    | rating hand <= 322 = FullHouse
    | rating hand <= 1599 = Flush
    | rating hand <= 1609 = Straight
    | rating hand <= 2467 = ThreeOfAKind
    | rating hand <= 3325 = TwoPair
    | rating hand <= 6185 = OnePair
    | otherwise = HighCard

choose :: [b] -> Int -> [[b]]
_      `choose` 0       = [[]]
[]     `choose` _       =  []
(x:xs) `choose` k       =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k


