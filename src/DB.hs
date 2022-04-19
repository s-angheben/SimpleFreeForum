{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}


module DB where

import Types
import           Control.Monad.IO.Class                     (liftIO)
import           Control.Monad.Logger                       (runStderrLoggingT, MonadLogger, runFileLoggingT)
import           Control.Monad.Reader
import           Data.Text
import           Data.Time
import           Database.Esqueleto.Experimental
import           Database.Persist.Postgresql                (ConnectionString, withPostgresqlConn, withPostgresqlPool, PostgresConf (PostgresConf))
import           Database.Persist.TH                        ( AtLeastOneUniqueKey(..)
       , OnlyOneUniqueKey(..)
       , mkMigrate
       , mkPersist
       , persistLowerCase
       , share
       , sqlSettings
       )

-- TYPES

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DbComment 
    topic Text 
    text Text
    time UTCTime
    deriving Show
|]


toComment :: DbComment -> Comment 
toComment (DbComment topic text time) = Comment (Topic topic) (CommentText text) time


fromComment :: Comment -> DbComment
fromComment (Comment (Topic topic) (CommentText text) time) = DbComment topic text time

-- DB

queryGetComments :: (MonadIO m, MonadLogger m)
         => Text -> SqlReadT m [Entity DbComment]
queryGetComments t =
    select $ do
    c <- from $ table @DbComment
    where_ (c ^. DbCommentTopic ==. val t)
    pure c


queryGetTopics :: (MonadIO m, MonadLogger m) => SqlReadT m [Value Text]
queryGetTopics =
    select $ distinct $ do
        c <- from $ table @DbComment
        let topics = c ^. DbCommentTopic
        return (c ^. DbCommentTopic)


conn = "host=localhost dbname=forum_db user=tester password=test_password port=5432"

{-
data DBEnv = DBEnv {
    logfile :: String,
    connection :: String
}

newtype AppDB a = AppDB { runAppDB' :: ReaderT DBEnv IO a }

runAppDB :: DBEnv -> AppDB a -> IO a
runAppDB env appDB = runReaderT (runAppDB' appDB) env 

-}

runQuery logfile conn query = runFileLoggingT logfile $ withPostgresqlConn conn (runSqlConn query)


setupDb :: (MonadIO m, MonadLogger m)
          => SqlPersistT m ()
setupDb = do
  runMigration migrateAll

initDB :: IO ()
initDB = runQuery "log.txt" conn setupDb

getTopics :: IO [Topic]
getTopics = do
    a <- liftIO $ runQuery "log.txt" conn queryGetTopics
    let c = unValue <$> a
    return $ Topic <$> c


getComment :: Topic -> IO [Comment]
getComment t = do
    a <- runFileLoggingT "log.txt" $ withPostgresqlConn conn (runSqlConn $ queryGetComments (getTopic t))
    let c = entityVal <$> a
    return $ toComment <$> c


addComment :: Comment -> IO ()
addComment c = do
    runFileLoggingT "log.txt" $ withPostgresqlConn conn (runSqlConn $ insert (fromComment c))
    return ()