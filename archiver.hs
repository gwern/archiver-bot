import Control.Concurrent (threadDelay)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust, fromMaybe)
import Network.Browser (browse, formToRequest, request, Form(..))
import Network.HTTP (getRequest, rspBody, simpleHTTP, RequestMethod(POST))
import Network.URI (isURI, parseURI, uriPath)
import System.Environment

import System.INotify

main :: IO ()
main = do args <- getArgs
          case args of
           (f:[]) ->   watch Nothing f
           (f:e:[]) -> watch (Just e) f
           _ -> error "must supply a filename or a filename and an email adress"

watch :: Maybe String -> FilePath -> IO ()
watch user file = do i <- initINotify
                     _ <- addWatch i [AllEvents] file (\_ -> archivePage user file)
                     return ()

archivePage :: Maybe String -> FilePath -> IO ()
archivePage user file = do contents <- B.readFile file
                           let (url,rest) = B.break (=='\n') contents
                           checkArchive email (B.unpack url)
                           print url
                           -- banned >=100 requests/hour; choke to ~1/minute
                           threadDelay 40000000 -- ~40 seconds
                           when (B.length rest /= 0) $ (B.writeFile file (B.drop 1 rest) >> archivePage user file) -- drop to get rid of leading \n
                           return ()
                           where
                               email = fromMaybe "nobody@mailinator.com" user

-- | Error check the URL and then archive it both ways
checkArchive :: String -> String -> IO ()
checkArchive email url = when (isURI url) (webciteArchive email url >> alexaArchive url)

webciteArchive :: String -> String -> IO ()
webciteArchive email url = openURL ("http://www.webcitation.org/archive?url=" ++ url ++ "&email=" ++ email)
                           >> return ()
   where openURL = simpleHTTP . getRequest

alexaArchive :: String -> IO ()
alexaArchive url = do let archiveform = Form POST
                             (fromJust $ parseURI "http://www.alexa.com/help/crawlrequest")
                                 [("url", url), ("submit", "")]
                      (uri, resp) <- browse $ request $ formToRequest archiveform
                      when (uriPath uri /= "/help/crawlthanks") $
                           error $ "Request failed! Alexa changed webpages? Response:" ++ rspBody resp
