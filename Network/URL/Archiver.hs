module Network.URL.Archiver (checkArchive) where

import Control.Monad (when, void)
import Data.Char (isAlphaNum, isAscii)
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import Network.Browser (browse, formToRequest, request, Form(..))
import Network.HTTP (RequestMethod(POST))
import Network.HTTP.Conduit (simpleHttp)
import Network.URI (isURI, parseURI)
import Text.Printf (printf)

-- | Open a URL
pingURL :: String -> IO ()
pingURL = void . simpleHttp

-- | Error check the URL and then archive it using 'wikiwixArchive', and 'internetArchiveLive'; excludes Tor links.
checkArchive ::  String -- ^ URL to archive
                -> IO ()
checkArchive url = when (isURI url && not (".onion/" `isInfixOf` url)) (internetArchiveLive url >> wikiwixArchive url >> googleSearch url >> archiveisArchive url)

-- | Request a URL through Internet Archive's on-demand archiving URL.
--
-- This also does a backup archive attempt through the live Internet mirror;
-- this is completely speculative and may result in no archiving.
-- This method is a guess based on my use of their mirror and a banner that is sometimes inserted;
-- see <http://www.archive.org/post/380853/virus-operating-in-internet-archive>
internetArchiveLive :: String -> IO ()
internetArchiveLive url = pingURL("http://web.archive.org/save/"++url) >> pingURL ("http://liveweb.archive.org/"++url)

wikiwixArchive :: String -> IO ()
wikiwixArchive url = pingURL ("http://archive.wikiwix.com/cache/?url="++url)

-- | <https://blog.archive.ph/post/45031162768/can-you-recommend-the-best-method-script-so-i-may-batch>
archiveisArchive :: String -> IO ()
archiveisArchive url = do let archiveform = Form POST (fromJust $ parseURI "https://archive.ph/submit/") [("url", url), ("submit", "")]
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
