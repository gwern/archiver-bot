module Network.URL.Archiver (checkArchive) where

import Control.Monad (when)
import Data.Char (isAlphaNum, isAscii)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Network.Browser (browse, formToRequest, request, Form(..))
import Network.HTTP (getRequest, rspBody, simpleHTTP, RequestMethod(POST))
import Network.URI (isURI, parseURI, uriPath)
import System.Random (getStdGen, randomR)
import Text.Printf (printf)

-- | Open a URL, and return either the HTML source or an error.
-- openURL :: String -> IO (Result (Response String))
openURL = simpleHTTP . getRequest

-- | Error check the URL and then archive it using 'webciteArchive', 'alexaArchive', and 'alexaToolbar'
checkArchive :: String -- ^ email for WebCite to send status to
                -> String -- ^ URL to archive
                -> IO ()
checkArchive email url = when (isURI url) (alexaToolbar url >> webciteArchive email url >> alexaArchive url >> internetArchiveLive url >> wikiwixArchive url)

{- | Request <http://www.webcitation.org> to copy a supplied URL; WebCite does on-demand archiving, unlike Alexa/Internet Archive,
   and so in practice this is the most useful function. This function throws away any return status from WebCite (which may be changed
   in the future), so it is suggested that one test with a valid email address.
   This and 'alexArchive' ignore any attempt to archive the archive's existing pages, since that is useless.

   /Warning!/ WebCite has throttling mechanisms; if you request more than 100 URLs per hour, your IP may be banned! It is
   suggested that one sleep for \~30 seconds between each URL request. -}
webciteArchive :: String -> String -> IO ()
webciteArchive email url = when (not $ "http://www.webcitation.org" `isPrefixOf` url) $
                            void $ openURL ("http://www.webcitation.org/archive?url=" ++ url ++ "&email=" ++ email)
   where void = (>> return ()) -- TODO replace with Control.Monad.void in GHC7

-- | Request a URL through Internet Archive's live Internet mirror; this is completely speculative and may result in no archiving.
--   This method is a guess based on my use of their mirror and a banner that is sometimes inserted;
--   see <http://www.archive.org/post/380853/virus-operating-in-internet-archive>
internetArchiveLive :: String -> IO ()
internetArchiveLive url = openURL ("http://liveweb.archive.org/"++url) >> return ()

-- | Request <http://www.alexa.com> to spider a supplied URL. Alexa supplies the Internet Archive's caches.
alexaArchive :: String -> IO ()
alexaArchive url = when (not $ "http://www.archive.org" `isPrefixOf` url) $
                     do let archiveform = Form POST
                             (fromJust $ parseURI "http://www.alexa.com/help/crawlrequest")
                                 [("url", url), ("submit", "")]
                        (uri, resp) <- browse $ request $ formToRequest archiveform
                        when (uriPath uri /= "/help/crawlthanks") $
                           print $ "Request failed! Alexa changed webpages? Response:" ++ rspBody resp

-- | Ping Alexa's servers like the Toolbar does; this may or may not result in any archiving.
alexaToolbar :: String -> IO ()
alexaToolbar url = do gen <- getStdGen
                      let rint = fst $ randomR (1000::Int,20000) gen
                      let payload = "wid=" ++ show rint ++ "&ref=&url=" ++ escape url
                      _ <- openURL $ "http://data.alexa.com/data/SbADd155Tq0000?cli=10&ver=spkyf-1.5.0&dat=ns&cdt=rq=0&" ++ payload
                      return ()
             where escape :: String -> String
                   escape = concatMap escapeURIChar
                   escapeURIChar :: Char -> String
                   escapeURIChar c | isAscii c && isAlphaNum c = [c]
                                   | otherwise                = concatMap (printf "%%%02X") [c]

wikiwixArchive :: String -> IO ()
wikiwixArchive url = openURL ("http://archive.wikiwix.com/cache/?url="++url) >> return ()