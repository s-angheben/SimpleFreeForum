
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core1 where

import Servant.API
import Servant.Server
import Servant
import Network.Wai.Handler.Warp
import Control.Monad.Except
import Control.Monad.Reader

import Types
import Text.Blaze.Html (stringComment)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Time
import Data.Text as T

type GetTopicListAPI   = "list" :> Get '[JSON] [Topic]
type GetCommentListAPI = "view" :> ReqBody '[JSON] Topic :> Post '[JSON] [Comment]
type PostCommentAPI    = "add"  :> ReqBody '[JSON] Comment :> Post '[JSON] Topic

type ForumAPI = GetTopicListAPI
           :<|> GetCommentListAPI
           :<|> PostCommentAPI


forumAPI :: Proxy ForumAPI
forumAPI = Proxy



-- newtype Handler a = IO (Either ServerError a)
-- runHandler' :: ExceptT ServerError IO a

-- Reader String a -> Handler a

-- type Server api = ServerT api Handler

--handleGetTopicList :: Handler [Topic]
handleGetTopicList :: Handler [Topic]
handleGetTopicList = return [Topic "ciao"]

handleGetCommentList :: Topic -> Handler [Comment]
handleGetCommentList t = do
    stringcomm <- liftIO (BL.readFile "comments.txt")
    let mcomm = decode stringcomm
    case mcomm of
        Nothing -> throwError custom404Err
        Just c  -> return [c]

    where
        custom404Err = err404 { errBody = "myfile.txt just isn't there, please leave this server alone." }

--handlePostComment :: Comment -> Handler Topic
handlePostComment c = return $ Topic "ai"


forumServer :: Server ForumAPI
forumServer = handleGetTopicList
         :<|> handleGetCommentList
         :<|> handlePostComment

main = run 8080 (serve forumAPI forumServer)

--- test

forumServer' :: ServerT ForumAPI (ReaderT String (ExceptT ServerError IO))
forumServer' = handleGetTopicList'
         :<|> handleGetCommentList'
         :<|> handlePostComment'

handleGetTopicList' :: ReaderT String (ExceptT ServerError IO) [Topic]
handleGetTopicList' = return [Topic "cioa"]

handleGetCommentList' :: Topic -> ReaderT String (ExceptT ServerError IO) [Comment]
handleGetCommentList' t = do
    stringcomm <- liftIO (BL.readFile "comments.txt")
    t <- liftIO getCurrentTime
    return [Comment (Topic "ciao") (CommentText "hello") t]

handlePostComment' :: Comment -> ReaderT String (ExceptT ServerError IO) Topic
handlePostComment' c = asks (Topic . T.pack)

funToHandler :: (ReaderT String (ExceptT ServerError IO)) a -> Handler a
funToHandler t = Handler $ runReaderT t "env" 

main' = run 8080 $ serve forumAPI $ hoistServer forumAPI funToHandler forumServer'


---
data Error = Error
data Env = Env

newtype AppM e a = AppM (ReaderT Env (ExceptT e IO) a)
  deriving (Functor, Applicative, Monad, MonadError e, MonadReader Env)


runAppM :: AppM e a -> Env -> IO (Either e a)
runAppM (AppM m) env = runExceptT $ runReaderT m env


{-
conn = "host=localhost dbname=forum_db user=tester password=test_password port=5432"

aliftSqlPersistMPool y x = liftIO (runSqlPersistMPool y x)


mmain :: IO [Comment]
--mmain = runStderrLoggingT $ withPostgresqlPool conn 10 $ aliftSqlPersistMPool $ do
mmain = runStderrLoggingT $ withPostgresqlPool conn 10 $ \b -> liftIO (runSqlPersistMPool (do
        runMigration migrateAll
        now <- liftIO getCurrentTime
        personId <- insert $ Comment "test" "questo e' un altro commento" now
        comments <- (entityVal <$>) <$> queryGetComments "lol"
        topics <-  (unValue <$>) <$> queryGetTopics
        liftIO $ print topics
        liftIO (print comments)
        return comments) b)
-}