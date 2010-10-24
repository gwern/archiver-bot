import Control.Concurrent (threadDelay)
import Control.Monad (forever, liftM, when)
import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

import System.INotify (addWatch, initINotify, EventVariety(AllEvents))

import Network.URL.Archiver (checkArchive)

main :: IO ()
main = do args <- getArgs
          case args of
           (f:[]) ->   watch Nothing f
           (f:e:[]) -> watch (Just e) f
           _ -> error "must supply a filename or a filename and an email address"

watch :: Maybe String -> FilePath -> IO ()
watch user file = do i <- initINotify
                     archivePage user file -- empty out existing file, then we add a watch & sleep
                     _ <- addWatch i [AllEvents] file (\_ -> archivePage user file)
                     forever $ threadDelay maxBound -- sleep... forever

archivePage :: Maybe String -> FilePath -> IO ()
archivePage user file = do contents <- B.readFile file
                           let (url,rest) = B.break (=='\n') contents
                           checkArchive email (B.unpack url)
                           print url
                           -- banned >=100 requests/hour; choke to ~1/minute
                           threadDelay 40000000 -- ~40 seconds
                           when (B.length rest /= 0) (B.writeFile file (B.drop 1 rest) >> archivePage user file) -- drop to get rid of leading \n
                           return ()
                           where
                               email = fromMaybe "nobody@mailinator.com" user