module ARP.IPv4 where

import Data.Binary (Binary, get, put)
import Data.Binary.Get (Get, getWord8)
import Data.Binary.Put (Put, putWord8)
import Data.Word (Word8)
import Numeric (readDec)
import Text.Printf (printf)

data IPv4 = IPv4 Word8 Word8 Word8 Word8

instance Binary IPv4 where
    get = do
        a <- getWord8
        b <- getWord8
        c <- getWord8
        d <- getWord8
        return (IPv4 a b c d)

    put (IPv4 a b c d) = do
        putWord8 a
        putWord8 b
        putWord8 c
        putWord8 d

instance Read IPv4 where
    readsPrec _ str = [(IPv4 a b c d, rest4)]
        where
        [(a, '.':rest1)] = readDec str
        [(b, '.':rest2)] = readDec rest1
        [(c, '.':rest3)] = readDec rest2
        [(d,     rest4)] = readDec rest3

instance Show IPv4 where
    show (IPv4 a b c d) = printf "%d.%d.%d.%d" a b c d

newIPv4 :: String -> IPv4
newIPv4 = read
