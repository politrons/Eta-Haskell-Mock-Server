{-# LANGUAGE OverloadedStrings #-} -- Mandatory language overload to overload String
module ScottyHttpServer where

import Web.Scotty
import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON, encode,decode)
import GHC.Generics
import ModelTypes

import Data.ByteString.Lazy.Char8 (ByteString)
import Web.Scotty.Internal.Types (ScottyT, ActionT, Param, RoutePattern, Options, File)
import Data.Text.Lazy (Text)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status

port = 3000 :: Int

{-| Thanks to type class we define that any [User] is JSON serializable/deserializable.|-}
instance ToJSON User
instance FromJSON User

{-| Using [scotty] passing [port] and [routes] we define the http server-}
scottyServer :: IO ()
scottyServer = do
    print ("Starting Server at port " ++ show port)
    scotty port routes

{-| We define the routes thanks to REST operators [get, post, put, delete, patch] which expect to
    receive a [RoutePattern] as a path and a [ActionM] as the action of the request. Then we return a [ScottyM]-}
routes :: ScottyM()
routes = do get "/service" responseService
            get "/author" responseName
            get "/mock/endpoint" responseUsers
            get "/error" errorResponse
            get "/errorJson" errorJsonResponse
--            post "/mock/endpoint" createUser
--            put "/mock/endpoint" updateUser
--            delete "/mock/endpoint:id" deleteById

{-| We use [text] operator from scotty we render the response in text/plain-}
responseService :: ActionM ()
responseService = text "Mock server running over JVM"

responseName :: ActionM ()
responseName = text "Paul Perez Garcia"

{-| Thanks to Aeson library and encode, we can use [json] operator to allow us to encode object into json
    [liftAndCatchIO] operator is used to extract from the IO monad the type and add it to ActionM monad.|-}
responseUsers :: ActionM ()
responseUsers = do liftIO $ print ("Request received")
                   users <- liftAndCatchIO $ return $ [(User 1 "Paul")]
                   json (show users)


errorResponse :: ActionM ()
errorResponse = do liftIO $ print ("Request received")
                   users <- liftAndCatchIO $ return $ [(User 1 "Paul")]
                   Web.Scotty.status status500 >> text "Error response"

errorJsonResponse :: ActionM ()
errorJsonResponse = do liftIO $ print ("Request received")
                       users <- liftAndCatchIO $ return $ [(User 1 "Paul")]
                       Web.Scotty.status status401 >> json (show users)

--{-| This part of the program is really interested, we are using function where first we need to call insertUser
--    passing a [User] but we have a [Maybe User] so we use a functor [<*>] to extract the User from the Maybe.
--     Then we have [sequence] operator which does:
--    -- | Evaluate each monadic action in the structure from left to right, and collect the results.
--    Then finally we need to lift the response from insertUser  [IO OK] to [OK] and to do that we use
--    the operator [liftAndCatchIO] which does:
--    -- | Like 'liftIO', but catch any IO exceptions and turn them into Scotty exceptions.
---}
--createUser :: ActionM ()
--createUser =  do maybeUser <- getUserParam
--                 status <- liftAndCatchIO $ sequence $ insertUser <$> maybeUser
--                 json (show status)
--
--updateUser :: ActionM ()
--updateUser =  do maybeUser <- getUserParam
--                 status <- liftAndCatchIO $ sequence $ updateUserById <$> maybeUser
--                 json (show status)
--
--deleteById :: ActionM ()
--deleteById = do id <- param "id"
--                status <- liftAndCatchIO $ deleteUserById id
--                json (show status)
--
--{-| In scotty we have [body] operator to get the request body.
--    We also use [decode] operator to extract and transform from json to Maybe of type we specify in the type signature-}
--getUserParam :: ActionT Text IO (Maybe User)
--getUserParam = do requestBody <- body
--                  return (decode requestBody)


