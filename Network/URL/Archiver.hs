module Network.URL.Archiver where

import Control.Monad (when)
import Data.Maybe (fromJust)
import Network.Browser (browse, formToRequest, request, Form(..))
import Network.HTTP (getRequest, rspBody, simpleHTTP, RequestMethod(POST))
import Network.URI (isURI, parseURI, uriPath)


-- | Error check the URL and then archive it using 'webciteArchive' and 'alexaArchive'
checkArchive :: String -> String -> IO ()
checkArchive email url = when (isURI url) (webciteArchive email url >> alexaArchive url)

{- | Request <www.webcitation.org> to copy a supplied URL; WebCite does on-demand archiving, unlike Alexa/Internet Archive,
   and so in practice this is the most useful function.
 
   *Warning!* WebCite has throttling mechanisms; if you request more than 100 URLs per hour, your IP may be banned! It is
   suggested that one sleep for \~40 seconds between each URL request. -}
webciteArchive :: String -> String -> IO ()
webciteArchive email url = openURL ("http://www.webcitation.org/archive?url=" ++ url ++ "&email=" ++ email)
                           >> return ()
   where openURL = simpleHTTP . getRequest

-- | Request <www.alexa.com> to spider a supplied URL; Alexa supplies the Internet Archive's caches (<www.archive.org>).
alexaArchive :: String -> IO ()
alexaArchive url = do let archiveform = Form POST
                             (fromJust $ parseURI "http://www.alexa.com/help/crawlrequest")
                                 [("url", url), ("submit", "")]
                      (uri, resp) <- browse $ request $ formToRequest archiveform
                      when (uriPath uri /= "/help/crawlthanks") $
                           error $ "Request failed! Alexa changed webpages? Response:" ++ rspBody resp
