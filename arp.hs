module Main where

import ARP.ARP (ArpOperation(..), ArpPacket(..))
import ARP.MAC (MAC, broadcast, newMAC)
import ARP.IPv4 (IPv4, newIPv4)
import Data.Binary (put)
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as L
import Data.List (intersperse)
import Data.Word (Word16, Word8)
import Text.Printf (printf)

newArpRequest :: IPv4 -> ArpPacket MAC IPv4
newArpRequest tpa = ArpPacket htype ptype hlen plen op sha spa tha tpa
    where htype = ethernetHtype
          ptype = ipv4Ptype
          hlen  = ethernetHlen
          plen  = ipv4Plen
          op    = ArpRequest
          sha   = (newMAC "b3:a3:86:07:b6:3e")
          spa   = (newIPv4 "192.168.168.105")
          tha   = broadcast

ipv4Ptype :: Word16
ipv4Ptype = 0x0800

ipv4Plen :: Word8
ipv4Plen = 4

ethernetHtype :: Word16
ethernetHtype = 1

ethernetHlen :: Word8
ethernetHlen = 6

main :: IO ()
main = do
    let arp = newArpRequest (newIPv4 "192.168.168.114")
        arpByteString = runPut $ put arp
        arpPayload = L.unpack arpByteString
        arpString = foldl (++) "" (intersperse " " $ map (printf "%02x") arpPayload)
    putStrLn arpString
