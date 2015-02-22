module ARP.Ethernet where

import Data.Binary (Binary, get, put)
import Data.Binary.Get (Get, getWord8)
import Data.Binary.Put (Put, putWord8)
import Data.Word (Word8)
import Numeric (readHex)
import Text.Printf (printf)

data Ethernet = Ethernet Word8 Word8 Word8 Word8 Word8 Word8

instance Binary Ethernet where
    get = do
        a <- getWord8
        b <- getWord8
        c <- getWord8
        d <- getWord8
        e <- getWord8
        f <- getWord8
        return (Ethernet a b c d e f)

    put (Ethernet a b c d e f) = do
        putWord8 a
        putWord8 b
        putWord8 c
        putWord8 d
        putWord8 e
        putWord8 f

instance Read Ethernet where
    readsPrec _ str = [(Ethernet a b c d e f, rest6)]
        where
        [(a, ':':rest1)] = readHex str
        [(b, ':':rest2)] = readHex rest1
        [(c, ':':rest3)] = readHex rest2
        [(d, ':':rest4)] = readHex rest3
        [(e, ':':rest5)] = readHex rest4
        [(f,     rest6)] = readHex rest5

instance Show Ethernet where
    show (Ethernet a b c d e f) = printf "%02x:%02x:%02x:%02x:%02x:%02x" a b c d e f

newEthernet :: String -> Ethernet
newEthernet = read

broadcast :: Ethernet
broadcast = newEthernet "ff:ff:ff:ff:ff:ff"
