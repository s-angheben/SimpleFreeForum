{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Core where

import Servant.API
import NetWork.Wai.Handler.Warp         (run)
import Types

type GetTopicListAPI   = Get '[JSON] [Topic]
--type GetCommentListApi = "view" :> Capture "topic" Topic :> Post '[JSON] [Comment]
--type PostCommentApi    = "add"  :> Caputre ""



forumAPI :: Proxy GetTopicListAPI 
forumAPI = Proxy


handleGet = [Topic "ciao"]


main = run 8080 (serve forumAPI EmptyConfig handleGet)

