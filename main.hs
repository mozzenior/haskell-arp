import Network
import System.IO (hClose, hGetLine)

main :: IO ()
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 5566
    (hdl, host, port) <- accept sock

    putStrLn ("Accept connection from " ++ host ++ " " ++ show port)
    line <- hGetLine hdl
    putStrLn ("Read: " ++ line)

    hClose hdl
    sClose sock
