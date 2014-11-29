{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Binary
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import           Data.Vector.Binary
import           GHC.Generics (Generic)
import           Network.HTTP.Conduit
import           System.Environment (getEnv)
import           System.IO (hFlush, stdout)
import qualified Web.Authenticate.OAuth as OA
import           Web.Twitter.Conduit
import           Web.Twitter.Types (Status(..),
                                    Coordinates(..),
                                    Entities(..),
                                    HashTagEntity(..),
                                    Place(..),
                                    Entity(..),
                                    User(..),
                                    BoundingBox(..),
                                    MediaEntity(..),
                                    URLEntity(..),
                                    UserEntity(..),
                                    MediaSize(..),
                                    Contributor(..))
import qualified Web.Twitter.Types.Lens as TTL


data StatusDb = StatusDb {
  statuses :: V.Vector Status
  } deriving (Generic)

deriving instance Generic Status
deriving instance Generic Contributor
deriving instance Generic Coordinates
deriving instance Generic Entities
deriving instance Generic Place
deriving instance Generic (Entity a)
deriving instance Generic (HashMap k v)
deriving instance Generic HashTagEntity
deriving instance Generic User
deriving instance Generic BoundingBox
deriving instance Generic UserEntity
deriving instance Generic MediaEntity
deriving instance Generic URLEntity
deriving instance Generic MediaSize

instance Binary MediaSize
instance Binary User
instance Binary BoundingBox
instance Binary Place
instance Binary HashTagEntity
instance Binary (Entity HashTagEntity)
instance Binary (Entity URLEntity)
instance Binary (Entity MediaEntity)
instance Binary (Entity UserEntity)
instance Binary UserEntity
instance Binary URLEntity
instance Binary MediaEntity
instance Binary Entities
instance Binary Coordinates
instance Binary Contributor
instance Binary Status
instance Binary StatusDb

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
  where texty = T.concat [ T.pack . show $ status ^. TTL.statusId
                         , ": "
                         , status ^. TTL.statusUser . TTL.userScreenName
                         , ": "
                         , status ^. TTL.statusText
                         ]

saveStatus :: Status -> IO ()
saveStatus status = undefined

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
