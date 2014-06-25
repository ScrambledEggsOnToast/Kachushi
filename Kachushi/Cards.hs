module Kachushi.Cards 
(
    Card (..)
  , Suit (..)
  , Rank (..)
  , card
  , fullDeck
  , colorPutCard
) where

import Control.Applicative ((<$>), (<*>))
import Data.Word (Word32)
import Data.Bits ((.|.), shiftL)
import Data.Function (on)
import Control.DeepSeq (NFData (..))
import System.Console.ANSI (setSGR, SGR (SetColor, Reset), Color (Green, Cyan, Red, Blue), ConsoleLayer (Foreground), ColorIntensity (Dull))

---------------------------
--  Types
---------------------------

data Card = Card { binary :: !Word32, rank :: Rank, suit :: Suit }
instance Show Card where
    show (Card _ r s) = sr ++ ss
        where
            sr = case r of
                R2 -> "2"
                R3 -> "3"
                R4 -> "4"
                R5 -> "5"
                R6 -> "6"
                R7 -> "7"
                R8 -> "8"
                R9 -> "9"
                RT -> "T"
                RJ -> "J"
                RQ -> "Q"
                RK -> "K"
                RA -> "A"
            ss = case s of 
                C -> "c"
                D -> "d"
                H -> "h"
                S -> "s"
instance Ord Card where
    compare (Card _ r1 s1) (Card _ r2 s2) = if cr /= EQ then cr else cs
        where 
            cr = compare r1 r2
            cs = compare s1 s2
    a <= b = a == b || compare a b == LT 
instance NFData Card where
    rnf (Card b r s) = b `seq` r `seq` s `seq` ()
instance Read (Card) where
    readsPrec d [r,s] = [(card or os,"")] 
        where
            or = read [r]
            os = read [s]
    readsPrec _ _ = []
instance Eq Card where
    (==) = (==) `on` binary

data Suit = C | D | H | S deriving (Show, Eq, Ord, Enum)
instance Read (Suit) where
    readsPrec d r = o
        where 
            o = case r of 
                "c" -> [(C,"")]
                "d" -> [(D,"")]
                "h" -> [(H,"")]
                "s" -> [(S,"")]
                otherwise -> []

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | RT | RJ | RQ | RK | RA 
    deriving (Show, Eq, Ord, Enum)
instance Read (Rank) where
    readsPrec d r = o 
        where 
            o = case r of 
                "2" -> [(R2,"")]
                "3" -> [(R3,"")]
                "4" -> [(R4,"")]
                "5" -> [(R5,"")]
                "6" -> [(R6,"")]
                "7" -> [(R7,"")]
                "8" -> [(R8,"")]
                "9" -> [(R9,"")]
                "T" -> [(RT,"")]
                "J" -> [(RJ,"")]
                "Q" -> [(RQ,"")]
                "K" -> [(RK,"")]
                "A" -> [(RA,"")]
                otherwise -> []

---------------------------
--  Conversion from rank-suit representation to bit representation
---------------------------

rankBits :: Rank -> Word32
rankBits r = p .|. (fromIntegral n `shiftL` 8) .|. (1 `shiftL` (16+n))
    where
        p = case r of
            R2 -> 2
            R3 -> 3
            R4 -> 5
            R5 -> 7
            R6 -> 11
            R7 -> 13
            R8 -> 17
            R9 -> 19
            RT -> 23
            RJ -> 29
            RQ -> 31
            RK -> 37
            RA -> 41
        n = fromEnum r

suitBits :: Suit -> Word32
suitBits s = case s of 
    C -> 0x8000
    D -> 0x4000
    H -> 0x2000
    S -> 0x1000

bits :: Rank -> Suit -> Word32
bits r s = rankBits r .|. suitBits s

---------------------------
--  Constructors
---------------------------

card :: Rank -> Suit -> Card
card r s = Card (bits r s) r s

fullDeck :: [Card]
fullDeck = card <$> [R2 ..] <*> [C ..]



colorPutCard :: Card -> IO ()
colorPutCard c@(Card _ _ s) = do
    let color = [Green, Cyan, Red, Blue] !! (fromEnum s)
    setSGR [SetColor Foreground Dull color]
    putStr . show $ c
    setSGR [Reset]
