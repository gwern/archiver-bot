import Control.Concurrent (threadDelay)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust, fromMaybe)
import Network.Browser (browse, formToRequest, request, Form(..))
import Network.HTTP (getRequest, rspBody, simpleHTTP, RequestMethod(POST))
import Network.URI (isURI, parseURI, uriPath)
import System.Environment

import Network.Gitit.Interface (askUser, liftIO, processWithM, uEmail, Plugin(PreCommitTransform), Inline(Link))
import Text.Pandoc (defaultParserState, readMarkdown)

plugin :: Plugin
plugin = PreCommitTransform archivePage

-- archivePage :: (MonadIO m) => String -> ReaderT (Config, Maybe User) (StateT IO) String
archivePage x = do mbUser <- askUser
                   let email = case mbUser of
                        Nothing -> "nobody@mailinator.com"
                        Just u  -> uEmail u
                   let p = readMarkdown defaultParserState x
                   -- force evaluation and archiving side-effects
                   _p' <- liftIO $ processWithM (archiveLinks email) p
                   return x -- note: this is read-only - don't actually change page!

archiveLinks :: String -> Inline -> IO Inline
archiveLinks e x@(Link _ (uln, _)) = forkIO (checkArchive e uln) >> return x
archiveLinks _ x                   = return x

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
