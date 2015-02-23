module ARP.Ethernet where

import ARP.MAC (MAC)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (Get, getWord16be, getWord32be)
import Data.Binary.Put (putWord16be, putWord32be)
import Data.Word (Word16, Word32)

data Ethernet p = Ethernet
    { dst       :: MAC
    , src       :: MAC
    , etype     :: Word16
    , payload   :: p
    , csum      :: Word32
    }

instance (Binary p) => Binary (Ethernet p) where
    get = do
        dst     <- get :: Get MAC
        src     <- get :: Get MAC
        etype   <- getWord16be
        payload <- get :: (Binary p) => Get p
        csum    <- getWord32be
        return (Ethernet dst src etype payload csum)

    put (Ethernet dst src etype payload csum) = do
        put         dst
        put         src
        putWord16be etype
        put         payload
        putWord32be csum

etypeARP :: Word16
etypeARP = 0x0806
