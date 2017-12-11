{-# LANGUAGE UndecidableInstances #-}
module Aradia.Backend.Types.Guild where

import Control.Lens hiding ((.=))
import Control.Monad

import Data.Aeson
import Data.Aeson.Encoding (text)
import Data.Aeson.Types
import Data.Aeson.TH
import qualified Data.HashMap.Strict as M
import Data.Monoid
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)

import Data.Singletons
import Data.Singletons.TH

import Aradia.Backend.Utils
import Aradia.Backend.Types.Common
import Aradia.Backend.Types.User

-- orphan
instance (FromJSON (Demote k), SingKind k) => FromJSON (SomeSing k) where
  parseJSON v = toSing <$> parseJSON v

instance (ToJSON (Demote k), SingKind k) => ToJSON (SomeSing k) where
  toJSON (SomeSing sing) = toJSON $ fromSing sing
  toEncoding (SomeSing sing) = toEncoding $ fromSing sing

-- this instance is kind of weird
-- we can basically just short-circuit this with `parseJSON v = pure sing`, but that fucks things up:
-- any value would parse with the above definition, which is a disaster
-- so we parse as regular and then check for equality
instance (FromJSON (Demote (KindOf a)), SingKind (KindOf a), SingI a, SEq (KindOf a), Show (Sing a)) => FromJSON (Sing a) where
  parseJSON v = do
    SomeSing (some :: Sing b) <- parseJSON v
    case some %:== (sing :: Sing a) of
      STrue -> pure (sing :: Sing a)
      SFalse -> fail $ "parsed " ++ show v ++ " but expected " ++ show (sing :: Sing a)

instance (ToJSON (Demote (KindOf a)), SingKind (KindOf a)) => ToJSON (Sing a) where
  toJSON = toJSON . fromSing
  toEncoding = toEncoding . fromSing


mkEnum "NotificationLevel" [discordTypes|
  ALL_MESSAGES 0
  ONLY_MENTIONS 1
  |]

mkEnum "ExplicitContentFilterLevel" [discordTypes|
  DISABLED 0
  MEMBERS_WITHOUT_ROLES 1
  ALL_MEMBERS 2
  |]

mkEnum "MfaLevel" [discordTypes|
  NONE 0
  ELEVATED 1
  |]

mkEnum "VerificationLevel" [discordTypes|
  NONE 0
  LOW 1
  MEDIUM 2
  HIGH 3
  VERY_HIGH 4
  |]

mkEnum "ChannelType" [discordTypes|
  GUILD_TEXT 0
  DM 1
  GUILD_VOICE 2
  GROUP_DM 3
  GUILD_CATEGORY 4
  |]

mkEnum "MessageType" [discordTypes|
  DEFAULT 0
  RECIPIENT_ADD 1
  RECIPIENT_REMOVE 2
  CALL 3
  CHANNEL_NAME_CHANGE 4
  CHANNEL_ICON_CHANGE 5
  CHANNEL_PINNED_MESSAGE 6
  GUILD_MEMBER_JOIN 7
  |]

data PermissionOverwriteType = RoleOverwriteType | MemberOverwriteType
  deriving (Show, Eq, Enum)

genSingletons [''ChannelType, ''PermissionOverwriteType]
singEqInstances [''ChannelType, ''PermissionOverwriteType]

-- todo: replace Show with singletons 1.4.0
deriving instance Show (Sing (a :: ChannelType))
deriving instance Eq (Sing (a :: ChannelType))

deriving instance Show (Sing (a :: PermissionOverwriteType))
deriving instance Eq (Sing (a :: PermissionOverwriteType))

permissionOverwriteTypeToText :: PermissionOverwriteType -> Text
permissionOverwriteTypeToText RoleOverwriteType = "role"
permissionOverwriteTypeToText MemberOverwriteType = "member"

instance FromJSON PermissionOverwriteType where
  parseJSON = withText "PermissionOverwriteType" $ \case
    "role" -> pure RoleOverwriteType
    "member" -> pure MemberOverwriteType
    _ -> mzero

instance ToJSON PermissionOverwriteType where
  toJSON = String . permissionOverwriteTypeToText
  toEncoding = text . permissionOverwriteTypeToText


data TextChannel = TextChannel'
  { _textChannelId :: Snowflake Channel
  , _textChannelType :: Sing ChannelTypeGuildText -- ^ this parameter is always constant; fill it with `sing`
  , _textChannelName :: Text
  , _textChannelGuildId :: Snowflake Guild
  , _textChannelParentId :: Nullable (Snowflake Channel)
  , _textChannelPosition :: Int
  , _textChannelPermissionOverwrites :: Vector PermissionOverwrite
  , _textChannelTopic :: Text
  , _textChannelNsfw :: Bool
  , _textChannelLastMessageId :: Nullable (Snowflake Message)
  } deriving (Show, Eq)

data DmChannel = DmChannel'
  { _dmChannelId :: Snowflake Channel
  , _dmChannelType :: Sing ChannelTypeDm
  , _dmChannelLastMessageId :: Nullable (Snowflake Message)
  , _dmChannelRecipients :: Vector User
  } deriving (Show, Eq)

data VoiceChannel = VoiceChannel'
  { _voiceChannelId :: Snowflake Channel
  , _voiceChannelType :: Sing ChannelTypeGuildVoice
  , _voiceChannelName :: Text
  , _voiceChannelGuildId :: Snowflake Guild
  , _voiceChannelParentId :: Nullable (Snowflake Channel)
  , _voiceChannelPosition :: Int
  , _voiceChannelPermissionOverwrites :: Vector PermissionOverwrite
  , _voiceChannelBitrate :: Int
  , _voiceChannelUserLimit :: Int
  } deriving (Show, Eq)

data GroupDmChannel =  GroupDmChannel'
  { _groupDmChannelId :: Snowflake Channel
  , _groupDmChannelType :: Sing ChannelTypeGroupDm
  , _groupDmChannelOwnerId :: Maybe (Snowflake User)
  , _groupDmChannelApplicationId :: Maybe (Snowflake Application)
  , _groupDmChannelLastMessageId :: Nullable (Snowflake Message)
  , _groupDmChannelRecipients :: Vector User
  , _groupDmChannelIcon :: Nullable DiscordImage
  } deriving (Show, Eq)

data ChannelCategory = ChannelCategory'
  { _channelCategoryId :: Snowflake Channel
  , _channelCategoryType :: Sing ChannelTypeGuildCategory
  , _channelCategoryName :: Text
  , _channelCategoryGuildId :: Snowflake Guild
  , _channelCategoryParentId :: Nullable (Snowflake Channel)
  , _channelCategoryPosition :: Int
  , _channelCategoryPermissionOverwrites :: Vector PermissionOverwrite
  , _channelCategoryNsfw :: Bool
  } deriving (Show, Eq)

data Channel
  = TextChannel TextChannel
  | DmChannel DmChannel
  | VoiceChannel VoiceChannel
  | GroupDmChannel GroupDmChannel
  | ChannelCategory ChannelCategory
  deriving (Show, Eq)

data RoleOverwrite = RoleOverwrite'
  { _roleOverwriteId :: Snowflake Role
  , _roleOverwriteType :: Sing RoleOverwriteType
  , _roleOverwriteAllow :: Permissions
  , _roleOverwriteDeny :: Permissions
  } deriving (Show, Eq)

data MemberOverwrite = MemberOverwrite'
  { _memberOverwriteId :: Snowflake User
  , _memberOverwriteType :: Sing MemberOverwriteType
  , _memberOverwriteAllow :: Permissions
  , _memberOverwriteDeny :: Permissions
  } deriving (Show, Eq)

data PermissionOverwrite
  = RoleOverwrite RoleOverwrite
  | MemberOverwrite MemberOverwrite
  deriving (Show, Eq)

data Guild = Guild
  { _guildId :: Snowflake Guild
  , _guildName :: Text
  , _guildIcon :: DiscordImage
  , _guildSplash :: DiscordImage
  , _guildOwner :: Maybe Bool
  , _guildOwnerId :: Snowflake User
  , _guildPermissions :: Maybe Permissions
  , _guildRegion :: Text
  , _guildAfkChannelId :: Snowflake Channel
  , _guildAfkTimeout :: Int
  , _guildEmbedEnabled :: Bool
  , _guildEmbedChannelId :: Snowflake Channel
  , _guildVerificationLevel :: VerificationLevel
  , _guildDefaultMessageNotifications :: NotificationLevel
  , _guildExplicitContentFilter :: ExplicitContentFilterLevel
  , _guildRoles :: Vector Role
  , _guildEmojis :: Vector Emoji
  , _guildFeatures :: Vector Text
  , _guildMfaLevel :: MfaLevel
  , _guildApplicationId :: Nullable (Snowflake ())
  , _guildWidgetEnabled :: Bool
  , _guildWidgetChannelId :: Snowflake Channel
  , _guildSystemChannelId :: Nullable (Snowflake Channel)
  -- only sent with GUILD_CREATE. codify better?
  , _guildJoinedAt :: Maybe UTCTime
  , _guildLarge :: Maybe Bool
  , _guildUnavailable :: Maybe Bool
  , _guildMemberCount :: Maybe Int
  , _guildVoiceStates :: Maybe (Vector VoiceState)
  , _guildMembers ::  Maybe (Vector GuildMember)
  , _guildChannels ::  Maybe (Vector Channel)
  , _guildPresences ::  Maybe (Vector Presence)
  } deriving (Show, Eq)

data GuildMember = GuildMember
  { _guildMemberUser :: User
  , _guildMemberNick :: Maybe Text
  , _guildMemberRoles :: Vector (Snowflake Role)
  , _guildMemberJoinedAt :: UTCTime
  , _guildMemberDeaf :: Bool
  , _guildMemberMute :: Bool
  } deriving (Show, Eq)


-- ^ this data type is purely for receiving messages
data Message = Message
  { _messageId :: Snowflake Message
  , _messageType :: MessageType
  , _messageChannelId :: Snowflake Channel
  , _messageAuthor :: User -- ^ not a valid user if sent by a webhook
  , _messageContent :: Text
  , _messageTimestamp :: UTCTime
  , _messageEditedTimestamp :: Nullable UTCTime
  , _messageTts :: Bool
  , _messageMentionEveryone :: Bool
  , _messageMentions :: Vector User
  , _messageMentionRoles :: Vector (Snowflake Role)
  , _messageAttachments :: Vector Attachment
  , _messageEmbeds :: Vector Embed
  , _messageReactions :: Maybe (Vector Reaction)
  , _messageNonce :: Maybe (Snowflake ()) -- Snowflake Nonce?
  , _messagePinned :: Bool
  , _messageWebhookId :: Maybe Text -- text???
  } deriving (Show, Eq)

-- separate into .Emoji?
-- better encode partial objects?
data Emoji = Emoji
  { _emojiId :: Snowflake Emoji
  , _emojiName :: Text
  , _emojiRoles :: Maybe (Vector (Snowflake Role))
  , _emojiUser :: Maybe User
  , _emojiRequireColons :: Maybe Bool
  , _emojiMnanaged :: Maybe Bool
  } deriving (Show, Eq)

data Reaction = Reaction
  { _reactionCount :: Int
  , _reactionMe :: Bool
  , _reactionEmoji :: Emoji
  } deriving (Show, Eq)

-- ^ Construction of embeds should be via lenses and emptyEmbed
-- ^ technically, an embed with all Nothings is an error. but how to codify this?
-- ^ some fields that are present in received embeds are omitted on purpose. if you need them, tell me.
{-
For the embed object, you can set every field except type (it will be rich regardless of if you try to set it), provider, video, and any height, width, or proxy_url values for images.
-}
-- sigh. i'd have thought i can avoid non-bidirectional serialization. fuck.
data Embed = Embed
  { _embedTitle :: Maybe Text
  , _embedDescription :: Maybe Text
  , _embedUrl :: Maybe Text -- URI type?
  , _embedTimestamp :: Maybe UTCTime
  , _embedColor :: Maybe Color
  , _embedFooter :: Maybe EmbedFooter
  , _embedImage :: Maybe EmbedImage
  , _embedThumbnail :: Maybe EmbedThumbnail
  , _embedAuthor :: Maybe EmbedAuthor
  , _embedFields :: Maybe (Vector EmbedField)
  } deriving (Show, Eq)

-- nothing.
emptyEmbed :: Embed
emptyEmbed = Embed Nothing Nothing Nothing Nothing Nothing
             Nothing Nothing Nothing Nothing Nothing

data EmbedThumbnail = EmbedThumbnail
  { _embedThumbnailUrl :: Text
  } deriving (Show, Eq)


data EmbedImage = EmbedImage
  { _embedImageUrl :: Text
  } deriving (Show, Eq)

-- sort out Maybes later
data EmbedAuthor = EmbedAuthor
  { _embedAuthorName :: Text
  , _embedAuthorUrl :: Maybe Text
  , _embedAuthorIcoNUrl :: Maybe Text
  } deriving (Show, Eq)

data EmbedFooter = EmbedFooter
  { _embedFooterText :: Text
  , _embedFooterIconUrl :: Maybe Text
  } deriving (Show, Eq)

data EmbedField = EmbedField
  { _embedFieldName :: Text
  , _embedFieldValue :: Text
  , _embedFieldInline :: Bool
  } deriving (Show, Eq)

-- ^ this field is for received attachments only.
data Attachment = Attachment
  { _attachmentId :: Snowflake Attachment
  , _attachmentFilename :: Text
  , _attachmentSize :: Int
  , _attachmentUrl :: Text
  , _attachmentProxyUrl :: Text
  , _attachmentHeight :: Nullable Int
  , _attachmentWidth :: Nullable Int
  } deriving (Show, Eq)

data VoiceState = VoiceState
  { _voiceStateGuildId :: Maybe (Snowflake Guild)
  , _voiceStateChannelId :: Snowflake Channel
  , _voiceStateUserId :: Snowflake User
  , _voiceStateSessionId :: Snowflake () -- Session
  , _voiceStateDeaf :: Bool
  , _voiceStateMute :: Bool
  , _voiceStateSelfDeaf :: Bool
  , _voiceStateSelfMute :: Bool
  , _voiceStateSuppress :: Bool
  } deriving (Show, Eq)

-- ^ presences are too much of a pain to implement
type Presence = Object

makeLensesWith fixedTypeFields ''Emoji
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''Emoji

makeLensesWith fixedTypeFields ''Reaction
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''Reaction 

makeLensesWith fixedTypeFields ''Embed
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''Embed

makeLensesWith fixedTypeFields ''EmbedThumbnail
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''EmbedThumbnail

makeLensesWith fixedTypeFields ''EmbedImage
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''EmbedImage

makeLensesWith fixedTypeFields ''EmbedAuthor
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''EmbedAuthor

makeLensesWith fixedTypeFields ''EmbedFooter
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''EmbedFooter

makeLensesWith fixedTypeFields ''EmbedField
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''EmbedField

makeLensesWith fixedTypeFields ''Attachment
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''Attachment

makeLensesWith fixedTypeFields ''VoiceState
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''VoiceState

makeLensesWith fixedTypeFields ''Message
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''Message

makeLensesWith fixedTypeFields ''RoleOverwrite
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''RoleOverwrite
makeLensesWith fixedTypeFields ''MemberOverwrite
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''MemberOverwrite

makeClassyPrisms ''PermissionOverwrite
deriveJSON defaultOptions { sumEncoding = UntaggedValue } ''PermissionOverwrite

makeLensesWith fixedTypeFields ''TextChannel
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''TextChannel

makeLensesWith fixedTypeFields ''DmChannel
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''DmChannel

makeLensesWith fixedTypeFields ''VoiceChannel
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''VoiceChannel

makeLensesWith fixedTypeFields ''GroupDmChannel
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''GroupDmChannel

makeLensesWith fixedTypeFields ''ChannelCategory
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''ChannelCategory

makeClassyPrisms ''Channel
deriveJSON defaultOptions { sumEncoding = UntaggedValue } ''Channel

makeLensesWith fixedTypeFields ''Guild
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''Guild

makeLensesWith fixedTypeFields ''GuildMember
deriveApiFieldJSON defaultOptions { omitNothingFields = True } ''GuildMember
