{-# LANGUAGE ScopedTypeVariables #-}
import Control.Concurrent (threadDelay)
import qualified Control.Exception as CE (catch, IOException)
import Control.Monad (liftM, unless, when)
import Data.List (delete)
import qualified Data.Set as S (fromList, toList)
import Data.Maybe (fromMaybe)
import Network.HTTP (getRequest, simpleHTTP)
import Network.URI (isURI)
import System.Environment (getArgs)
import System.Process (runCommand)
import qualified Data.ByteString.Char8 as B (count, intercalate, length, readFile, singleton, split, unpack, writeFile, ByteString)
import System.Random

import Network.URL.Archiver (checkArchive)

main :: IO ()
main = do args <- getArgs
          case args of
           (f:[]) ->   archivePage f Nothing Nothing
           (f:e:[]) -> archivePage f (Just e) Nothing
           (f:e:s:[]) -> archivePage f (Just e) (Just s)
           _ -> error "must supply a filename or a filename and an email address"

archivePage :: FilePath -> Maybe String -> Maybe String -> IO ()
archivePage file email sh = do connectedp <- CE.catch (simpleHTTP (getRequest "http://www.webcitation.org")) (\(_::CE.IOException) -> return (Left undefined))
                               case connectedp of
                                 Left _  -> -- Left = ConnError, network not working! sleep for a minute and try again later
                                   threadDelay 90000000 >> archivePage file email sh
                                 Right _ -> do -- we have access to the WWW, it seems. proceeding with mission!
                                   contents <- B.readFile file
                                   when (B.length contents == 0) $ threadDelay 90000000
                                   (url,rest) <- splitRandom contents
                                   let url' = B.unpack url
                                   let email' =  fromMaybe "nobody@mailinator.com" email
                                   when (isURI url') $ do checkArchive email' url'
                                                          print url'
                                                          maybe (return ()) (\x -> runCommand (x ++ " " ++ url') >> return ()) sh
                                                          -- banned >=100 requests/hour; choke it
                                                          threadDelay 26000000 -- ~26 seconds
                                   unless (null rest) (writePages file url >> archivePage file email sh) -- rid of leading \n

-- re-reads a possibly modified 'file' from disk, removes the archived URL from it, and writes it back out for 'archivePage' to read immediately
writePages :: FilePath -> B.ByteString -> IO ()
writePages file done = do original <- liftM (B.split '\n') $ B.readFile file
                          let new =  S.toList $ S.fromList original
                          let final = B.intercalate (B.singleton '\n') $ filter (not . (== done)) new
                          B.writeFile file final

-- pick a random entry in the list
splitRandom :: B.ByteString -> IO (B.ByteString, [B.ByteString])
splitRandom s = do let ss = B.split '\n' s
                   let l  = B.count '\n' s
                   i <- getStdRandom (randomR (0,l))
                   let randpick = if length ss > 1 then ss !! i else ss !! 0
                   let removed = Data.List.delete randpick ss
                   return (randpick, removed)
