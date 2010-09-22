import Control.Concurrent (threadDelay)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust, fromMaybe)
import Network.Browser (browse, formToRequest, request, Form(..))
import Network.HTTP (getRequest, rspBody, simpleHTTP, RequestMethod(POST))
import Network.URI (isURI, parseURI, uriPath)
import System.Environment

main :: IO ()
main = do args <- getArgs
          case args of
           (f:[]) -> archivePage Nothing f
           (f:e:[]) -> archivePage (Just e) f
           _ -> error "must supply a filename or a filename and an email adress"
          
archivePage :: Maybe String -> FilePath -> IO ()
archivePage user file = loop
                    where
                        email = fromMaybe "nobody@mailinator.com" user
                        loop = do contents <- B.readFile file
                                  let (url,rest) = B.break (=='\n') contents
                                  checkArchive email (B.unpack url)
                                  print url
                                  -- banned >=100 requests/hour; choke to ~1/minute
                                  threadDelay 40000000 -- ~40 seconds
                                  if B.length rest /= 0 then do B.writeFile file (B.drop 1 rest) -- drop to get rid of leading \n
                                                                loop
                                   else return ()

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
