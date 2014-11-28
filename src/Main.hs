{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens

tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey = error "Provide consumer key!"
    , oauthConsumerSecret = error "Provide consumer secret!"
    }

authorize :: (MonadBaseControl IO m, MonadResource m)
          => OAuth -- ^ OAuth Consumer key and secret
          -> (String -> m String) -- ^ PIN prompt
          -> Manager
          -> m Credential
authorize oauth getPIN mgr = do
    cred <- getTemporaryCredential oauth mgr
    let url = authorizeUrl oauth cred
    pin <- getPIN url
    getAccessToken oauth (insert "oauth_verifier" (B8.pack pin) cred) mgr

getTWInfo :: IO TWInfo
getTWInfo = do
    cred <- withManager $ \mgr -> authorize tokens getPIN mgr
    return $ setCredential tokens cred def
  where
    getPIN url = liftIO $ do
        putStrLn $ "browse URL: " ++ url
        putStr "> what was the PIN twitter provided you with? "
        hFlush stdout
        getLine

main :: IO ()
main = do
    twInfo <- getTWInfo
    putStrLn $ "# your home timeline (up to 100 tweets):"
    withManager $ \mgr -> do
      -- listsStatuses (ListNameParam "thimura/haskell")
      -- homeTimeline
      -- userTimeline (ScreenNameParam "thimura")
      sourceWithMaxId twInfo mgr $ userTimeline (ScreenNameParam "thimura")
      C.$= CL.isolate 100
      C.$$ CL.mapM_ $ \status -> liftIO $ do
        TIO.putStrLn $ T.concat [ T.pack . show $ status ^. statusId
                                , ": "
                                , status ^. statusUser . userScreenName
                                , ": "
                                , status ^. statusText
                                ]
