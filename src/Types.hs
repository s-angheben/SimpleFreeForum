module Types where

import Data.Text (Text)
import Data.Time 

-- TOPIC type
newtype Topic = Topic Text
    deriving Show

-- COMMENT type
newtype CommentText = CommentText Text
    deriving Show
newtype CommentId   = CommentId Int
    deriving Show

data Comment = Comment
  { commentId    :: CommentId
  , commentTopic :: Topic
  , commentText  :: CommentText
  , commentTime  :: UTCTime
  }
  deriving Show