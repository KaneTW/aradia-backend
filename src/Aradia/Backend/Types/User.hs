module Aradia.Backend.Types.User where

import Control.Lens

import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.TH
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import qualified Data.Text.Read as TR
import Data.Vector (Vector)

import Text.Printf

import Aradia.Backend.Utils
import Aradia.Backend.Types.Common

newtype Discriminator = Discriminator Int
  deriving (Eq, Ord)

instance Show Discriminator where
  show (Discriminator x) = printf "%04d" x

instance FromJSON Discriminator where
  parseJSON (String txt) = case TR.decimal txt of
    Right (n,_) -> pure $ Discriminator n
    Left err -> fail err

instance ToJSON Discriminator where
  toJSON = toJSON . show
  toEncoding = string . show

data User = User
  { _userId :: Snowflake User
  , _userUsername :: Text
  , _userDiscriminator :: Discriminator
  , _userAvatar :: DiscordImage
  , _userBot :: Maybe Bool
  , _userMfaEnabled :: Maybe Bool
  , _userVerified :: Maybe Bool
  , _userEmail :: Maybe Text
  } deriving (Show, Eq)

data Connection = Connection
  { _connectionId :: Snowflake Connection
  , _connectionName :: Text
  , _connectionType :: Text -- docs says "twitch, youtube" but don't codify it
  , _connectionRevoked :: Bool
  , _connectionIntegrations :: Vector Integration
  } deriving (Show, Eq)

data Integration = Integration
  { _integrationId :: Snowflake Integration
  , _integrationName :: Text
  , _integrationType :: Text -- see _connectionType
  , _integrationEnabled :: Bool
  , _integrationSyncing :: Bool
  , _integrationRoleId :: Snowflake Role
  , _integrationExpireBehavior :: Int -- ???
  , _integrationExpireGracePeriod :: Int
  , _integrationUser :: User
  , _integrationAccount :: IntegrationAccount
  , _integrationSyncedAt :: UTCTime
  } deriving (Show, Eq)

data IntegrationAccount = IntegrationAccount
  { _integrationAccountId :: Snowflake IntegrationAccount
  , _integrationAccountName :: Text
  } deriving (Show, Eq)

makeLensesWith fixedTypeFields ''User
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''User

makeLensesWith fixedTypeFields ''Connection
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''Connection

makeLensesWith fixedTypeFields ''Integration
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''Integration

makeLensesWith fixedTypeFields ''IntegrationAccount
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''IntegrationAccount
