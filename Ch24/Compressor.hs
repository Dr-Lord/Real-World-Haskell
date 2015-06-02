-- File: Real World Haskell\Ch24\Compressor.hs
import Control.Concurrent (forkIO)
import Control.Exception
import qualified Data.ByteString.Lazy as L
import System.Console.Readline (readline)
import Codec.Compression.GZip (compress)

main = do
    maybeLine <- readline "Enter a file to compress>"
    case maybeLine of
        Nothing -> return () -- user entered EOF
        Just "" -> return () -- treat no name (blank) as "want to quit"
        Just name -> do
            handle (print :: SomeException -> IO ()) $ do
                content <- L.readFile name
                forkIO (compressFile name content)
                return ()
            main
        where compressFile path = L.writeFile (path ++ ".gz") . compress
