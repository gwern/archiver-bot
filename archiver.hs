import Control.Concurrent (threadDelay)
import Control.Monad (forever, liftM, when)
import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import Network.HTTP (getRequest, simpleHTTP)
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as B (break, intercalate, length, readFile, singleton, split, unpack, writeFile, ByteString)

import System.INotify (addWatch, initINotify, EventVariety(AllEvents))

import Network.URL.Archiver (checkArchive)

main :: IO ()
main = do args <- getArgs
          case args of
           (f:[]) ->   watch Nothing f
           (f:e:[]) -> watch (Just e) f
           _ -> error "must supply a filename or a filename and an email address"

watch :: Maybe String -> FilePath -> IO ()
watch usr file = do i <- initINotify
                    archivePage usr file -- empty out existing file, then we add a watch & sleep
                    _ <- addWatch i [AllEvents] file (\_ -> archivePage usr file)
                    forever $ threadDelay maxBound -- sleep... forever. inotify will wake us up

archivePage :: Maybe String -> FilePath -> IO ()
archivePage usr file = do connectedp <- simpleHTTP (getRequest "http://www.google.com/")
                          case connectedp of
                             Left _  -> -- Left = ConnError, network not working! sleep for a minute and try again later
                                        threadDelay 90000000 >> archivePage usr file
                             Right _ -> do -- we have access to the WWW, it seems. proceeding with mission!
                                          contents <- B.readFile file
                                          let (url,rest) = B.break (=='\n') contents
                                          checkArchive email (B.unpack url)
                                          print url
                                          -- banned >=100 requests/hour; choke to ~1/minute
                                          threadDelay 40000000 -- ~40 seconds
                                          when (B.length rest /= 0) (writePages file url >> archivePage usr file) -- drop to get rid of leading \n
                                              where email = fromMaybe "nobody@mailinator.com" usr

-- re-reads a possibly modified 'file' from disk, removes the archived URL from it, and writes it back out for 'archivePage' to read immediately
writePages :: FilePath -> B.ByteString -> IO ()
writePages file done = do original <- liftM (B.split '\n') $ B.readFile file
                          let new = nub $ sort original
                          let final = B.intercalate (B.singleton '\n') $ filter (not . (== done)) new
                          B.writeFile file final