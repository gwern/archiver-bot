{-# LANGUAGE ScopedTypeVariables #-}
import Control.Concurrent (threadDelay)
import qualified Control.Exception as CE (catch, IOException)
import Control.Monad (liftM, when)
import Data.List (delete, nub, sort)
import Data.Maybe (fromMaybe)
import Network.HTTP (getRequest, simpleHTTP)
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as B (count, intercalate, readFile, singleton, split, unpack, writeFile, ByteString)
import System.Random

import Network.URL.Archiver (checkArchive)

main :: IO ()
main = do args <- getArgs
          case args of
           (f:[]) ->   archivePage Nothing f
           (f:e:[]) -> archivePage (Just e) f
           _ -> error "must supply a filename or a filename and an email address"

archivePage :: Maybe String -> FilePath -> IO ()
archivePage usr file = do connectedp <- CE.catch (simpleHTTP (getRequest "http://www.webcitation.org")) (\(_::CE.IOException) -> return (Left undefined))
                          case connectedp of
                             Left _  -> -- Left = ConnError, network not working! sleep for a minute and try again later
                                        threadDelay 90000000 >> archivePage usr file
                             Right _ -> do -- we have access to the WWW, it seems. proceeding with mission!
                                          contents <- B.readFile file
                                          (url,rest) <- splitRandom contents
                                          checkArchive email (B.unpack url)
                                          print url
                                          -- banned >=100 requests/hour; choke it
                                          threadDelay 26000000 -- ~26 seconds
                                          when (length rest /= 0) (writePages file url >> archivePage usr file) -- drop to get rid of leading \n
                                              where email = fromMaybe "nobody@mailinator.com" usr

-- re-reads a possibly modified 'file' from disk, removes the archived URL from it, and writes it back out for 'archivePage' to read immediately
writePages :: FilePath -> B.ByteString -> IO ()
writePages file done = do original <- liftM (B.split '\n') $ B.readFile file
                          let new = nub $ sort original
                          let final = B.intercalate (B.singleton '\n') $ filter (not . (== done)) new
                          B.writeFile file final

-- pick a random entry in the list
splitRandom :: B.ByteString -> IO (B.ByteString, [B.ByteString])
splitRandom s = do let ss = B.split '\n' s
                   let l  = B.count '\n' s
                   i <- getStdRandom (randomR (0,l))
                   let randpick = ss !! i
                   let removed = Data.List.delete randpick ss
                   return (randpick, removed)
