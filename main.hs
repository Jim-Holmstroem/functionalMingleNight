import Network (listenOn,sendTo, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)
import Data.Char

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = fromIntegral (read $ head args :: Int)
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ (head args)
    sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    forkIO $ commandProcessor handle
    sockHandler sock

commandProcessor :: Handle -> IO ()
commandProcessor handle = do
    line <- hGetLine handle
    let cmd = words line
        pea = head cmd
    if all isDigit pea && not (null pea)
        then let
           a = read pea::Integer
           monkey = show $ (+1) a --curry
          in do
           putStrLn monkey
           (sendTo donkey (PortNumber 1234)) . (++"\n") $ monkey --magic -> QED
           re
        else re
        where donkey = "192.168.48.45"
              re = commandProcessor handle

