{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Model (
  NoteContent(..), StorageId(..)
  , ChecklistContent(..), ChecklistItem(..)
  , AgendaContent(..)
  , TaskContent(..), RecurrenceRule(..), TaskReminder(..)
  , TaskConflict(..)
  , Content
  , Identifiable(..), hash
  , mkIdentifiable
  ) where


import Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Base64.Lazy as Base64 (encode)
import           Data.ByteString.Lazy.Char8  as BL
import qualified Data.UUID.V4               as UUID (nextRandom)
import qualified Data.UUID                  as UUID (toString)
import           Data.Digest.Pure.SHA        (bytestringDigest, sha256)
import           GHC.Generics                (Generic)

data StorageId = StorageId { id      :: !String
                           , version :: !String
                           } deriving (Show, Generic, Eq)

instance ToJSON   StorageId
instance FromJSON StorageId

class (Show a, Eq a, ToJSON a, FromJSON a) => Content a where
  hash :: a -> String

data Content a => Identifiable a =
    Identifiable { storageId :: !StorageId, content :: !a } deriving (Eq, Show)

instance Content a => ToJSON (Identifiable a) where
  toJSON (Identifiable {..}) =
    object [ "storageId" .= storageId, "content" .= content ]

instance Content a => FromJSON (Identifiable a) where
  parseJSON = withObject "Identifiable" $ \value -> Identifiable
    <$> value .: "storageId"
    <*> value .: "content"

mkIdentifiable :: (Content a) => a -> IO (Identifiable a)
mkIdentifiable content = do
    uuid <- liftIO $ fmap UUID.toString UUID.nextRandom
    return $ Identifiable (StorageId { id = uuid, version = hash content }) content

-- ===================== Note =============================================

data NoteContent = NoteContent { title       :: Maybe String
                               , noteContent :: String
                               } deriving (Show, Generic, Eq)

instance Content NoteContent where
  hash content = base64Sha256 $ show content

instance FromJSON NoteContent
instance ToJSON   NoteContent

-- ======================= CHECKLIST =======================================

data ChecklistContent = ChecklistContent { name  :: String
                                         , items :: [ChecklistItem]
                                         } deriving (Show, Generic, Eq)

data ChecklistItem    = ChecklistItem    { label   :: String
                                         , checked :: Bool
                                         } deriving (Show, Generic, Eq)

instance FromJSON ChecklistContent
instance ToJSON   ChecklistContent

instance FromJSON ChecklistItem
instance ToJSON   ChecklistItem

instance Content ChecklistContent where
  hash checklistContent = base64Sha256 $ show checklistContent

-- ======================= AGENDA ==========================================

data AgendaContent = AgendaContent
  { agendaName :: String
  , agendaDescription :: Maybe String
  , agendaTimezone :: String
  } deriving (Show, Generic, Eq)

instance FromJSON AgendaContent where
  parseJSON = withObject "AgendaContent" $ \v -> AgendaContent
    <$> v .: "name"
    <*> v .:? "description"
    <*> v .: "timezone"

instance ToJSON AgendaContent where
  toJSON AgendaContent { agendaName = agendaName', agendaDescription = agendaDescription', agendaTimezone = agendaTimezone' } =
    object [ "name" .= agendaName'
           , "description" .= agendaDescription'
           , "timezone" .= agendaTimezone'
           ]

instance Content AgendaContent where
  hash agendaContent = base64Sha256 $ show agendaContent

-- ======================= TASK ============================================

data RecurrenceRule = RecurrenceRule
  { frequency :: String
  , interval :: Int
  , untilUtc :: Maybe String
  , count :: Maybe Int
  , byWeekday :: Maybe [String]
  , byMonthday :: Maybe [Int]
  } deriving (Show, Generic, Eq)

instance FromJSON RecurrenceRule
instance ToJSON RecurrenceRule

data TaskReminder = TaskReminder
  { offsetMinutesBefore :: Int
  } deriving (Show, Generic, Eq)

instance FromJSON TaskReminder
instance ToJSON TaskReminder

data TaskContent = TaskContent
  { taskAgendaId :: String
  , taskTitle :: String
  , taskDescription :: Maybe String
  , taskStatus :: String
  , taskScheduledStartUtc :: String
  , taskScheduledEndUtc :: String
  , taskTimezone :: String
  , taskEstimatedDurationMinutes :: Maybe Int
  , taskTags :: [String]
  , taskRecurrence :: Maybe RecurrenceRule
  , taskReminders :: [TaskReminder]
  , taskDeletedAt :: Maybe String
  } deriving (Show, Generic, Eq)

instance FromJSON TaskContent where
  parseJSON = withObject "TaskContent" $ \v -> TaskContent
    <$> v .: "agendaId"
    <*> v .: "title"
    <*> v .:? "description"
    <*> v .: "status"
    <*> v .: "scheduledStartUtc"
    <*> v .: "scheduledEndUtc"
    <*> v .: "timezone"
    <*> v .:? "estimatedDurationMinutes"
    <*> v .:? "tags" .!= []
    <*> v .:? "recurrence"
    <*> v .:? "reminders" .!= []
    <*> v .:? "deletedAt"

instance ToJSON TaskContent where
  toJSON TaskContent
      { taskAgendaId = taskAgendaId'
      , taskTitle = taskTitle'
      , taskDescription = taskDescription'
      , taskStatus = taskStatus'
      , taskScheduledStartUtc = taskScheduledStartUtc'
      , taskScheduledEndUtc = taskScheduledEndUtc'
      , taskTimezone = taskTimezone'
      , taskEstimatedDurationMinutes = taskEstimatedDurationMinutes'
      , taskTags = taskTags'
      , taskRecurrence = taskRecurrence'
      , taskReminders = taskReminders'
      , taskDeletedAt = taskDeletedAt'
      } =
    object [ "agendaId" .= taskAgendaId'
           , "title" .= taskTitle'
           , "description" .= taskDescription'
           , "status" .= taskStatus'
           , "scheduledStartUtc" .= taskScheduledStartUtc'
           , "scheduledEndUtc" .= taskScheduledEndUtc'
           , "timezone" .= taskTimezone'
           , "estimatedDurationMinutes" .= taskEstimatedDurationMinutes'
           , "tags" .= taskTags'
           , "recurrence" .= taskRecurrence'
           , "reminders" .= taskReminders'
           , "deletedAt" .= taskDeletedAt'
           ]

instance Content TaskContent where
  hash taskContent = base64Sha256 $ show taskContent

data TaskConflict = TaskConflict
  { leftTaskId :: String
  , rightTaskId :: String
  } deriving (Show, Generic, Eq)

instance FromJSON TaskConflict
instance ToJSON TaskConflict

-- =============================== Utils ==========================================

base64Sha256 :: String -> String
base64Sha256 contentToHash = BL.unpack . Base64.encode . bytestringDigest . sha256 $ BL.pack contentToHash
