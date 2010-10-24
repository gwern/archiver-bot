module Network.URL.Archiver where

import Control.Monad (when)
import Data.Maybe (fromJust)
import Network.Browser (browse, formToRequest, request, Form(..))
import Network.HTTP (getRequest, rspBody, simpleHTTP, RequestMethod(POST))
import Network.URI (isURI, parseURI, uriPath)


-- | Error check the URL and then archive it using 'webciteArchive' and 'alexaArchive'
checkArchive :: String -- ^ email for WebCite to send status to 
                -> String -- ^ URL to archive
                -> IO ()
checkArchive email url = when (isURI url) (webciteArchive email url >> alexaArchive url)

{- | Request <http://www.webcitation.org> to copy a supplied URL; WebCite does on-demand archiving, unlike Alexa/Internet Archive,
   and so in practice this is the most useful function. This function throws away any return status from WebCite (which may be changed
   in the future), so it is suggested that one test with a valid email address.
 
   /Warning!/ WebCite has throttling mechanisms; if you request more than 100 URLs per hour, your IP may be banned! It is
   suggested that one sleep for \~40 seconds between each URL request. -}
webciteArchive :: String -> String -> IO ()
webciteArchive email url = openURL ("http://www.webcitation.org/archive?url=" ++ url ++ "&email=" ++ email)
                           >> return ()
   where openURL = simpleHTTP . getRequest

-- |  Request <http://www.alexa.com> to spider a supplied URL. Alexa supplies the Internet Archive's caches.
-- TODO: currently broken? Alexa changed pages? is down?
alexaArchive :: String -> IO ()
alexaArchive url = do let archiveform = Form POST
                             (fromJust $ parseURI "http://www.alexa.com/help/crawlrequest")
                                 [("url", url), ("submit", "")]
                      (uri, resp) <- browse $ request $ formToRequest archiveform
                      when (uriPath uri /= "/help/crawlthanks") $
                           print $ "Request failed! Alexa changed webpages? Response:" ++ rspBody resp
