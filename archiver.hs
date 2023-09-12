{-# LANGUAGE ScopedTypeVariables #-}
import Control.Concurrent (threadDelay)
import qualified Control.Exception as CE (catch, IOException)
import Control.Monad (liftM, unless, void, when)
import Data.List (delete)
import qualified Data.Set as S (fromList, toList)
import Data.Maybe (fromMaybe)
import Network.HTTP (getRequest, simpleHTTP)
import Network.URI (isURI)
import System.Environment (getArgs)
import System.Process (runCommand, terminateProcess)
import qualified Data.ByteString.Char8 as B (length, lines, readFile, unlines, unpack, writeFile, ByteString)
import System.Random (getStdRandom, randomR)

import Network.URL.Archiver (checkArchive)

main :: IO ()
main = do args <- getArgs
          case args of
           (f:[])       -> archivePage f Nothing  Nothing
           (f:e:[])     -> archivePage f (Just e) Nothing
           (f:e:n:[]) -> archivePage f (Just e) (Just (read n :: Int))
           _ -> error "must supply a filename"

archivePage :: FilePath -> Maybe String -> Maybe Int -> IO ()
archivePage file sh n = do -- default: 48 seconds (converted to milliseconds)
                                 let n' = 1000000 * fromMaybe 48 n
                                 let loop = archivePage file sh n
                                 connectedp <- CE.catch (simpleHTTP (getRequest "http://www.webcitation.org")) (\(_::CE.IOException) -> return (Left undefined))
                                 case connectedp of
                                   Left _  -> -- Left = ConnError, network not working! sleep for a minute and try again later
                                     threadDelay n' >> loop
                                   Right _ -> do -- we have access to the WWW, it seems. proceeding with mission!
                                     contents <- B.readFile file
                                     when (B.length contents == 0) $ threadDelay n'
                                     (url,rest) <- splitRandom contents
                                     let url' = B.unpack url
                                     when (isURI url') $ do
                                                            print url'
                                                            hdl <- case sh of
                                                                        Nothing -> return Nothing
                                                                        Just sh' -> return $ Just (runCommand (sh' ++ " '" ++ url' ++ "'"))
                                                            -- banned >=100 requests/hour; choke it
                                                            threadDelay n'
                                                            case hdl of
                                                              Nothing -> return ()
                                                              Just hdl' -> void $ liftM terminateProcess hdl' -- GC
                                     unless (null rest) (writePages file url >> loop) -- rid of leading \n

-- re-reads a possibly modified 'file' from disk, removes the archived URL from it, and writes it back out for 'archivePage' to read immediately
writePages :: FilePath -> B.ByteString -> IO ()
writePages file done = do original <- liftM B.lines $ B.readFile file
                          let sorted =  S.toList $ S.fromList original
                          let final = B.unlines $ filter (not . (== done)) sorted
                          B.writeFile file final

-- pick a random entry in the list
splitRandom :: B.ByteString -> IO (B.ByteString, [B.ByteString])
splitRandom s = do let ss = B.lines s
                   let l  = length ss
                   i <- getStdRandom (randomR (0,l))
                   let randpick = if length ss > 1 then ss !! i else head ss
                   let removed = Data.List.delete randpick ss
                   return (randpick, removed)
