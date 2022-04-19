{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}

module Types where

import Data.Text (Text)
import Data.Time 
import Data.Aeson
import GHC.Generics

-- TOPIC type
newtype Topic = Topic Text
    deriving (Show, ToJSON, FromJSON)


getTopic :: Topic -> Text 
getTopic (Topic t) = t

-- COMMENT type
newtype CommentText = CommentText Text
    deriving (Show, ToJSON, FromJSON)

data Comment = Comment
  { commentTopic :: Topic
  , commentText  :: CommentText
  , commentTime  :: UTCTime
  }
  deriving (Show, Generic)

instance ToJSON Comment where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Comment

exampleCommentString = "{\"commentTopic\":\"ciao\",\"commentText\":\"hello\",\"commentTime\":\"2022-04-16T15:32:48.459784824Z\"}"

