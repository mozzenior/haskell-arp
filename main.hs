import Network.Socket
import System.IO (hClose, hGetLine)

main :: IO ()
main = withSocketsDo $ do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet (fromInteger 5566) iNADDR_ANY)
    listen sock 5
    (client, cliAddr) <- accept sock

    let (SockAddrInet (PortNum port) addr) = cliAddr
    addrS <- inet_ntoa addr
    putStrLn ("Accept connection from " ++ addrS ++ " " ++ show port)

    line <- recv client 128
    putStrLn ("Read: " ++ line)

    sClose client
    sClose sock
