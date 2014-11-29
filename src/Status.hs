{-# LANGUAGE DeriveGeneric #-}

module Status where

import           GHC.Generics (Generic)
import           Web.Twitter.Conduit hiding (Status)

data Status = Status
    { statusContributors :: Maybe [Contributor]
    , statusCoordinates :: Maybe Coordinates
    , statusCreatedAt :: DateString
    , statusCurrentUserRetweet :: Maybe UserId
    , statusEntities :: Maybe Entities
    , statusExtendedEntities :: Maybe Entities
    , statusFavoriteCount :: Integer
    , statusFavorited :: Maybe Bool
    , statusFilterLevel :: Maybe Text
    , statusId :: StatusId
    , statusInReplyToScreenName :: Maybe Text
    , statusInReplyToStatusId :: Maybe StatusId
    , statusInReplyToUserId :: Maybe UserId
    , statusLang :: Maybe LanguageCode
    , statusPlace :: Maybe Place
    , statusPossiblySensitive :: Maybe Bool
    , statusScopes :: Maybe Object
    , statusRetweetCount :: Integer
    , statusRetweeted :: Maybe Bool
    , statusRetweetedStatus :: Maybe Status
    , statusSource :: Text
    , statusText :: Text
    , statusTruncated :: Bool
    , statusUser :: User
    , statusWithheldCopyright :: Maybe Bool
    , statusWithheldInCountries :: Maybe [Text]
    , statusWithheldScope :: Maybe Text
    } deriving (Show, Eq)
