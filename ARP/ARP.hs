module ARP.ARP where

import Data.Binary (Binary, get, put)
import Data.Binary.Get (Get, getWord16be, getWord8)
import Data.Binary.Put (Put, putWord16be, putWord8)
import Data.Word (Word16, Word8)

data ArpOperation = ArpRequest
                  | ArpReply
                  deriving (Show)

instance Binary ArpOperation where
    get = do
        op <- getWord8
        case op of
            0x01        -> return ArpRequest
            0x02        -> return ArpReply
            otherwise   -> error "Invalid ARP operation"

    put op = case op of
        ArpRequest  -> putWord8 0x01
        ArpReply    -> putWord8 0x02

data ArpPacket hw p = ArpPacket
    { arpHtype  :: Word16
    , arpPtype  :: Word16
    , arpHlen   :: Word8
    , arpPlen   :: Word8
    , arpOper   :: ArpOperation
    , arpSha    :: hw
    , arpSpa    :: p
    , arpTha    :: hw
    , arpTpa    :: p
    } deriving (Show)

instance (Binary hw, Binary p) => Binary (ArpPacket hw p) where
    get = do
        htype <- getWord16be
        ptype <- getWord16be
        hlen  <- getWord8
        plen  <- getWord8
        op    <- (get :: Get ArpOperation)
        sha   <- (get :: (Binary hw) => Get hw)
        spa   <- (get :: (Binary  p) => Get  p)
        tha   <- (get :: (Binary hw) => Get hw)
        tpa   <- (get :: (Binary  p) => Get  p)
        return (ArpPacket htype ptype hlen plen op sha spa tha tpa)

    put arp = do
        putWord16be (arpHtype arp)
        putWord16be (arpPtype arp)
        putWord8    (arpHlen  arp)
        putWord8    (arpPlen  arp)
        put         (arpOper  arp)
        put         (arpSha   arp)
        put         (arpSpa   arp)
        put         (arpTha   arp)
        put         (arpTpa   arp)
