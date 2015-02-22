import Data.Binary.Get (Get, getWord16be, getWord8)
import Data.Binary.Put (Put, putWord16be, putWord8)
import Data.Word (Word16, Word8)
import Numeric (readDec, readHex)
import Text.Printf (printf)

data ArpOperation = ArpRequest
                  | ArpReply
                  deriving (Show)

data Ethernet = Ethernet Word8 Word8 Word8 Word8 Word8 Word8

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

data IPv4 = IPv4 Word8 Word8 Word8 Word8

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

newArpRequest :: IPv4 -> ArpPacket Ethernet IPv4
newArpRequest tpa = ArpPacket htype ptype hlen plen op sha spa tha tpa
    where htype = ethernetHtype
          ptype = ipv4Ptype
          hlen  = ethernetHlen
          plen  = ipv4Plen
          op    = ArpRequest
          sha   = (newEthernet "b3:a3:86:07:b6:3e")
          spa   = (newIPv4 "192.168.168.105")
          tha   = ethernetBroadcast

getArpOperation :: Get ArpOperation
getArpOperation = do
    op <- getWord8
    case op of
        0x01        -> return ArpRequest
        0x02        -> return ArpReply
        otherwise   -> error "Invalid ARP operation"

putArpOperation :: ArpOperation -> Put
putArpOperation op = case op of
    ArpRequest  -> putWord8 0x01
    ArpReply    -> putWord8 0x02

getEthernet :: Get Ethernet
getEthernet = do
    a <- getWord8
    b <- getWord8
    c <- getWord8
    d <- getWord8
    e <- getWord8
    f <- getWord8
    return (Ethernet a b c d e f)

putEthernet :: Ethernet -> Put
putEthernet (Ethernet a b c d e f) = do
    putWord8 a
    putWord8 b
    putWord8 c
    putWord8 d
    putWord8 e
    putWord8 f

getIPv4 :: Get IPv4
getIPv4 = do
    a <- getWord8
    b <- getWord8
    c <- getWord8
    d <- getWord8
    return (IPv4 a b c d)

putIPv4 :: IPv4 -> Put
putIPv4 (IPv4 a b c d) = do
    putWord8 a
    putWord8 b
    putWord8 c
    putWord8 d

getArpPacket :: Get hw -> Get p -> Get (ArpPacket hw p)
getArpPacket getHw getP = do
    htype <- getWord16be
    ptype <- getWord16be
    hlen  <- getWord8
    plen  <- getWord8
    op    <- getArpOperation
    sha   <- getHw
    spa   <- getP
    tha   <- getHw
    tpa   <- getP
    return (ArpPacket htype ptype hlen plen op sha spa tha tpa)

putArpPacket :: (hw -> Put)-> (p -> Put) -> ArpPacket hw p -> Put
putArpPacket putHw putP arp = do
    putWord16be     (arpHtype arp)
    putWord16be     (arpPtype arp)
    putWord8        (arpHlen  arp)
    putWord8        (arpPlen  arp)
    putArpOperation (arpOper  arp)
    putHw           (arpSha   arp)
    putP            (arpSpa   arp)
    putHw           (arpTha   arp)
    putP            (arpTpa   arp)

ethernetHtype :: Word16
ethernetHtype = 1

ipv4Ptype :: Word16
ipv4Ptype = 0x0800

ethernetHlen :: Word8
ethernetHlen = 6

ipv4Plen :: Word8
ipv4Plen = 4

ethernetBroadcast :: Ethernet
ethernetBroadcast = newEthernet "ff:ff:ff:ff:ff:ff"

ipv4Broadcast :: IPv4
ipv4Broadcast = newIPv4 "255.255.255.255"

-- main :: IO ()
-- main = return ()
