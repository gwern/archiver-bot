module Network.URL.Archiver (checkArchive) where

import Control.Monad (when, unless, void)
import Data.Char (isAlphaNum, isAscii)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromJust)
import Network.Browser (browse, formToRequest, request, Form(..))
import Network.HTTP (getRequest, simpleHTTP, RequestMethod(POST))
import Network.URI (isURI, parseURI)
import System.Random (getStdGen, randomR)
import Text.Printf (printf)

-- | Open a URL
pingURL :: String -> IO ()
pingURL = void . simpleHTTP . getRequest

-- | Error check the URL and then archive it using 'webciteArchive', 'wikiwixArchive', 'internetArchiveLive', and 'alexaToolbar'; excludes Tor links.
checkArchive :: String -- ^ email for WebCite to send status to
                -> String -- ^ URL to archive
                -> IO ()
checkArchive email url = when (isURI url && not (".onion/" `isInfixOf` url)) (alexaToolbar url >> webciteArchive email url >> internetArchiveLive url >> wikiwixArchive url >> googleSearch url >> archiveisArchive url)

{- | Request <http://www.webcitation.org> to copy a supplied URL; WebCite does on-demand archiving, unlike Alexa/Internet Archive,
   and so in practice this is the most useful function. This function throws away any return status from WebCite (which may be changed
   in the future), so it is suggested that one test with a valid email address.
   This ignores any attempt to archive the archive's existing pages, since that is useless.

   /Warning!/ WebCite has throttling mechanisms; if you request more than 100 URLs per hour, your IP may be banned! It is
   suggested that one sleep for \~30 seconds between each URL request. -}
webciteArchive :: String -> String -> IO ()
webciteArchive email url = unless ("http://www.webcitation.org" `isPrefixOf` url) $
                            pingURL ("http://www.webcitation.org/archive?url="++url++ "&email="++email)

-- | Request a URL through Internet Archive's on-demand archiving URL.
--
-- This also does a backup archive attempt through the live Internet mirror;
-- this is completely speculative and may result in no archiving.
-- This method is a guess based on my use of their mirror and a banner that is sometimes inserted;
-- see <http://www.archive.org/post/380853/virus-operating-in-internet-archive>
internetArchiveLive :: String -> IO ()
internetArchiveLive url = pingURL("http://web.archive.org/save/"++url) >> pingURL ("http://liveweb.archive.org/"++url)

-- | Ping Alexa's servers like the Toolbar does; this may or may not result in any archiving.
alexaToolbar :: String -> IO ()
alexaToolbar url = do gen <- getStdGen
                      let rint = fst $ randomR (1000::Int,20000) gen
                      let payload = "wid=" ++ show rint ++ "&ref=&url=" ++ escape url
                      pingURL ("http://data.alexa.com/data/SbADd155Tq0000?cli=10&ver=spkyf-1.5.0&dat=ns&cdt=rq=0&"++payload)
                      return ()

wikiwixArchive :: String -> IO ()
wikiwixArchive url = pingURL ("http://archive.wikiwix.com/cache/?url="++url)

-- | <http://blog.archive.is/post/45031162768/can-you-recommend-the-best-method-script-so-i-may-batch>
archiveisArchive :: String -> IO ()
archiveisArchive url = do let archiveform = Form POST (fromJust $ parseURI "http://archive.today/submit/") [("url", url), ("submit", "")]
                          void $ browse $ request $ formToRequest archiveform

-- can't hurt to let Google know it exists
googleSearch :: String -> IO ()
googleSearch url = pingURL ("http://www.google.com/search?q=" ++ escape url)

-- | Utility function to URL-encode a string for use in URL arguments; copied from somewhere
escape :: String -> String
escape = concatMap escapeURIChar
escapeURIChar :: Char -> String
escapeURIChar c | isAscii c && isAlphaNum c = [c]
                | otherwise                = concatMap (printf "%%%02X") [c]
