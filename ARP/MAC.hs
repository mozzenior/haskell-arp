module ARP.MAC where

import Data.Binary (Binary, get, put)
import Data.Binary.Get (Get, getWord8)
import Data.Binary.Put (Put, putWord8)
import Data.Word (Word8)
import Numeric (readHex)
import Text.Printf (printf)

data MAC = MAC Word8 Word8 Word8 Word8 Word8 Word8

instance Binary MAC where
    get = do
        a <- getWord8
        b <- getWord8
        c <- getWord8
        d <- getWord8
        e <- getWord8
        f <- getWord8
        return (MAC a b c d e f)

    put (MAC a b c d e f) = do
        putWord8 a
        putWord8 b
        putWord8 c
        putWord8 d
        putWord8 e
        putWord8 f

instance Read MAC where
    readsPrec _ str = [(MAC a b c d e f, rest6)]
        where
        [(a, ':':rest1)] = readHex str
        [(b, ':':rest2)] = readHex rest1
        [(c, ':':rest3)] = readHex rest2
        [(d, ':':rest4)] = readHex rest3
        [(e, ':':rest5)] = readHex rest4
        [(f,     rest6)] = readHex rest5

instance Show MAC where
    show (MAC a b c d e f) = printf "%02x:%02x:%02x:%02x:%02x:%02x" a b c d e f

newMAC :: String -> MAC
newMAC = read

broadcast :: MAC
broadcast = newMAC "ff:ff:ff:ff:ff:ff"
