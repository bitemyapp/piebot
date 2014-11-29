{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Applicative ((<$>))
import           Control.Lens hiding ((|>))
import           Control.Monad.IO.Class
import           Control.Monad.Reader (ask)
import           Control.Monad.State (get, put)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Acid
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.SafeCopy
import           Data.Sequence
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Typeable (Typeable)
import           Network.HTTP.Conduit
import           System.Environment (getEnv)
import           System.IO (hFlush, stdout)
import qualified Web.Authenticate.OAuth as OA
import           Web.Twitter.Conduit
import           Web.Twitter.Types.Lens


data StatusDb = StatusDb {
  statuses :: Seq ReducedStatus
  } deriving (Show, Typeable)

data ReducedStatus = ReducedStatus {
    rdsText     :: T.Text
  , rdsUserName :: T.Text
  , rdsLang     :: String
  } deriving (Show, Typeable)

resetStatuses :: Update StatusDb ()
resetStatuses = put $ StatusDb Data.Sequence.empty

addStatus :: ReducedStatus -> Update StatusDb ()
addStatus status = do
  StatusDb statuses <- get
  put $ StatusDb (statuses |> status)

viewStatuses :: Query StatusDb (Seq ReducedStatus)
viewStatuses = do
  StatusDb statuses <- ask
  return statuses

$(deriveSafeCopy 0 'base ''StatusDb)
$(deriveSafeCopy 0 'base ''ReducedStatus)
$(makeAcidic ''StatusDb ['addStatus, 'viewStatuses, 'resetStatuses])


authorize :: (MonadBaseControl IO m, MonadResource m)
          => OA.OAuth -- ^ OAuth Consumer key and secret
          -> (String -> m String) -- ^ PIN prompt
          -> Manager
          -> m OA.Credential
authorize oauth getPIN mgr = do
    cred <- OA.getTemporaryCredential oauth mgr
    let url = OA.authorizeUrl oauth cred
    pin <- getPIN url
    OA.getAccessToken oauth
      (OA.insert "oauth_verifier" (B8.pack pin) cred) mgr

getTWInfo :: IO TWInfo
getTWInfo = do
  key <- getEnv "OAUTH_KEY"
  secret <- getEnv "OAUTH_SECRET"
  let tokens = twitterOAuth {
          OA.oauthConsumerKey = B8.pack key
        , OA.oauthConsumerSecret = B8.pack secret
        }
  cred <- withManager $ \mgr -> authorize tokens getPIN mgr
  return $ setCredential tokens cred OA.def
  where
    getPIN url = liftIO $ do
        putStrLn $ "browse URL: " ++ url
        putStr "> what was the PIN twitter provided you with? "
        hFlush stdout
        getLine

printStatus :: Status -> IO ()
printStatus status = TIO.putStrLn texty
  where texty = T.concat [ T.pack . show $ status ^. statusId
                         , ": "
                         , status ^. statusUser . userScreenName
                         , ": "
                         , status ^. statusText
                         ]

saveStatus :: Status -> IO ()
saveStatus status = undefined

-- Prelude> let exampleStatus = ReducedStatus "Hello, World!" "argumatronic" "EN"
-- Prelude> sdb <- openLocalStateFrom "db/" (StatusDb Data.Sequence.empty)
-- Prelude> Data.Acid.update sdb (AddStatus exampleStatus)
-- ()
-- Prelude> statuses <- query sdb ViewStatuses
-- Prelude> statuses
-- fromList [ReducedStatus {rdsText = "Hello, World!", rdsUserName = "argumatronic", rdsLang = "EN"}]
-- Prelude> Data.Acid.update sdb (AddStatus exampleStatus)
-- ()
-- Prelude> statuses <- query sdb ViewStatuses
-- Prelude> statuses
-- fromList [ReducedStatus {rdsText = "Hello, World!", rdsUserName = "argumatronic", rdsLang = "EN"},ReducedStatus {rdsText = "Hello, World!", rdsUserName = "argumatronic", rdsLang = "EN"}]

main :: IO ()
main = do
    twInfo <- getTWInfo
    putStrLn $ "# your home timeline (up to 100 tweets):"
    withManager $ \mgr -> do
      -- listsStatuses (ListNameParam "thimura/haskell")
      -- homeTimeline
      -- userTimeline (ScreenNameParam "thimura")
      sourceWithMaxId twInfo mgr $
        userTimeline (ScreenNameParam "argumatronic")
      C.$= CL.isolate 100
      C.$$ CL.mapM_ $ \status -> liftIO (printStatus status)
