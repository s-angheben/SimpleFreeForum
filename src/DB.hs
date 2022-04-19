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
DBComment 
    topic Text 
    text Text
    time UTCTime
    deriving Show
|]


toComment :: DBComment -> Comment 
toComment (DBComment topic text time) = Comment (Topic topic) (CommentText text) time

-- DB

queryGetComments :: (MonadIO m, MonadLogger m)
         => Text -> SqlReadT m [Entity DBComment]
queryGetComments t =
    select $ do
    c <- from $ table @DBComment
    where_ (c ^. DBCommentTopic ==. val t)
    pure c


queryGetTopics :: (MonadIO m, MonadLogger m) => SqlReadT m [Value Text]
queryGetTopics =
    select $ distinct $ do
        c <- from $ table @DBComment
        let topics = c ^. DBCommentTopic
        return (c ^. DBCommentTopic)


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


getTopics :: IO [Topic]
getTopics = do
    a <- liftIO $ runQuery "log.txt" conn queryGetTopics
    let c = unValue <$> a
    return $ Topic <$> c


getComment :: Text -> IO [Comment]
getComment t = do
    a <- runFileLoggingT "log.txt" $ withPostgresqlConn conn (runSqlConn $ queryGetComments t)
    let c = entityVal <$> a
    return $ toComment <$> c
