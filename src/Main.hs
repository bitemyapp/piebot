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
import qualified Data.Map.Strict as Map
import           Data.SafeCopy
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
  statuses :: Map.Map StatusId ReducedStatus
  } deriving (Show, Typeable)

type TweetText = T.Text
type Username  = T.Text

data ReducedStatus = ReducedStatus {
    rdsText     :: TweetText
  , rdsUserName :: Username
    -- type LanguageCode = String
  , rdsLang     :: Maybe LanguageCode
    -- Integer
  , rdsStatusId :: StatusId
  } deriving (Show, Typeable)

resetStatuses :: Update StatusDb ()
resetStatuses = put $ StatusDb Map.empty

addStatus :: ReducedStatus -> Update StatusDb ()
addStatus status = do
  StatusDb statuses <- get
  put $ StatusDb (Map.insert k status statuses)
  where k = rdsStatusId status

viewStatuses :: Query StatusDb (Map.Map StatusId ReducedStatus)
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

toReducedStatus :: Status -> ReducedStatus
toReducedStatus status = rds
  where rds = ReducedStatus txt user lang sid
        txt  = status ^. statusText
        user = status ^. statusUser . userScreenName
        lang = status ^. statusLang
        sid  = status ^. statusId

saveStatus :: AcidState StatusDb -> Status -> IO ()
saveStatus myDb status = do
  Data.Acid.update myDb (AddStatus (toReducedStatus status))
  return ()

-- Prelude> let exampleStatus = ReducedStatus "Hello, World!" "argumatronic" "EN" 1
-- Prelude> sdb <- openLocalStateFrom "db/" (StatusDb Map.empty)
-- Prelude> Data.Acid.update sdb (AddStatus exampleStatus)
-- ()
-- Prelude> statuses <- query sdb ViewStatuses
-- Prelude> statuses
-- fromList [ReducedStatus {rdsText = "Hello, World!", rdsUserName = "argumatronic", rdsLang = "EN"}]
-- Prelude> Data.Acid.update sdb (AddStatus exampleStatus)
-- ()
-- Prelude> statuses <- query sdb ViewStatuses
-- Prelude> statuses
-- fromList [ReducedStatus {rdsText = "Hello, World!", rdsUserName = "argumatronic", rdsLang = "EN", rdsStatusId = 1},ReducedStatus {rdsText = "Hello, World!", rdsUserName = "argumatronic", rdsLang = "EN", rdsStatusId = 1}]

foldStream :: TWInfo
           -> AcidState StatusDb
           -> Int
           -> UserParam
           -> IO (Map.Map StatusId ReducedStatus)
foldStream twInfo sdb numTweets user = do
  withManager $ \mgr -> do
      sourceWithMaxId twInfo mgr $
        userTimeline user
      C.$= CL.isolate numTweets
      -- C.$$ CL.mapM_ $ \status -> liftIO (saveStatus sdb status)
      C.$$ CL.foldM reduceStatus Map.empty
  where reduceStatus m status = return $ Map.insert k rds m
          where rds = toReducedStatus status
                k   = rdsStatusId rds

main :: IO ()
main = do
    twInfo <- getTWInfo
    putStrLn "Opening database"
    myDb <- openLocalStateFrom "db/" (StatusDb Map.empty)
    m <- foldStream twInfo myDb 100 (ScreenNameParam "argumatronic")
    print m
