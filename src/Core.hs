{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Core where

import Servant.API
import Servant.Server
import Servant
import Network.Wai.Handler.Warp
import Control.Monad.Except
import Control.Monad.Reader

import Text.Blaze.Html (stringComment)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Time
import Data.Text as T
import DB
import Types


instance ToHttpApiData Topic where
  toUrlPiece = getTopic 

instance FromHttpApiData Topic where
  parseUrlPiece = Right . Topic


type GetTopicListAPI   = "list" :> Get '[JSON] [Topic]
type GetCommentListAPI = Capture "topic" Topic :> "view" :> Get '[JSON] [Comment]
type PostCommentAPI    = Capture "topic" Topic :> "add"  :> ReqBody '[JSON] CommentText :> Post '[JSON] Topic

type ForumAPI = GetTopicListAPI
           :<|> GetCommentListAPI
           :<|> PostCommentAPI


forumAPI :: Proxy ForumAPI
forumAPI = Proxy



-- newtype Handler a = IO (Either ServerError a)
-- runHandler' :: ExceptT ServerError IO a

-- Reader String a -> Handler a

-- type Server api = ServerT api Handler

forumServer :: ServerT ForumAPI (AppM ServerError)
forumServer = handleGetTopicList
         :<|> handleGetCommentList
         :<|> handlePostComment

handleGetTopicList :: (AppM ServerError) [Topic]
handleGetTopicList = liftIO getTopics

handleGetCommentList :: Topic -> (AppM ServerError) [Comment]
handleGetCommentList (Topic t) = do
    com <- liftIO $ getComment t
    return com
    where
        custom404Err = err404 { errBody = "myfile.txt just isn't there, please leave this server alone." }
--    return [Comment (CommentId 4) (Topic "ciao") (CommentText "hello") t]

handlePostComment :: Topic -> CommentText -> (AppM ServerError) Topic
handlePostComment t c = asks (Topic . T.pack)

funToHandler :: (AppM ServerError) a -> Handler a
funToHandler (AppM t) = Handler $ runReaderT t "env"

main = run 8080 $ serve forumAPI $ hoistServer forumAPI funToHandler forumServer


---
data Error = Error
type Env = String

newtype AppM e a = AppM (ReaderT Env (ExceptT e IO) a)
  deriving (Functor, Applicative, Monad, MonadError e, MonadReader Env, MonadIO)


runAppM :: AppM e a -> Env -> IO (Either e a)
runAppM (AppM m) env = runExceptT $ runReaderT m env

