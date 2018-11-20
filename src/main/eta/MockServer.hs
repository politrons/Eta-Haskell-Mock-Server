{-# LANGUAGE OverloadedStrings #-} -- Mandatory language overload to overload String
{-# LANGUAGE DeriveGeneric #-}
module MockServer where

import Web.Scotty
import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON, encode,decode)
import GHC.Generics
import Data.ByteString.Lazy.Char8 (ByteString)
import Web.Scotty.Internal.Types (ScottyT, ActionT, Param, RoutePattern, Options, File)
import Data.Text.Lazy (Text)
import Control.Concurrent (myThreadId,newEmptyMVar,forkIO,threadDelay,putMVar,takeMVar)
import Data.IORef (newIORef,IORef,atomicModifyIORef,readIORef,writeIORef)
import Control.Monad.IO.Class (liftIO)
import Text.Read (lift)
import Network.HTTP.Types.Status
import Data.IORef (newIORef,IORef,atomicModifyIORef,readIORef,writeIORef)
import ModelTypes
import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Lazy.Char8 (ByteString)

port = 3000 :: Int

{-| Using [scotty] passing [port] and [routes] we define the http server-}
scottyServer :: IO ()
scottyServer = do
    print ("Starting Server at port " ++ show port)
    statusRef <- newIORef 200 -- We create default response status
    responseBodyRef <- newIORef "{}" -- We create default response body
    delayRef <- newIORef 0 -- We create default response time
    scotty port (routes statusRef responseBodyRef delayRef)

{-| We define the routes thanks to REST operators [get, post, put, delete, patch] which expect to
    receive a [RoutePattern] as a path and a [ActionM] as the action of the request. Then we return a [ScottyM]-}
routes :: IORef Int -> IORef String -> IORef Int -> ScottyM()
routes statusRef responseBodyRef delayRef= do  get "/service" responseService
                                               get "/author" responseName
                                               get "/mock/endpoint" (responseMockBody statusRef responseBodyRef delayRef)
                                               put "/setResponse/status/:id/delay/:delay" (statusResponse statusRef responseBodyRef delayRef)

{-| We use [text] operator from scotty we render the response in text/plain-}
responseService :: ActionM ()
responseService = text "Haskell Mock server running over JVM"

responseName :: ActionM ()
responseName = text "Paul Perez Garcia"

{-| Thanks to Aeson library and encode, we can use [json] operator to allow us to encode object into json
    [liftAndCatchIO] operator is used to extract from the IO monad the type and add it to ActionM monad.|-}
responseMockBody :: IORef Int -> IORef String -> IORef Int -> ActionM ()
responseMockBody statusRef responseBodyRef delayRef= do  liftIO $ print ("Request to be mocked received")
                                                         status <- liftAndCatchIO $ readIORef statusRef
                                                         responseBody <- liftAndCatchIO $ readIORef responseBodyRef
                                                         delayResponse <- liftAndCatchIO $ readIORef delayRef
                                                         _ <- liftAndCatchIO $ threadDelay delayResponse
                                                         Web.Scotty.status (transformStatusCodeToStatus status) >>  json responseBody

{-| We change the value of status for the futures request.-}
statusResponse :: IORef Int -> IORef String -> IORef Int ->  ActionM ()
statusResponse statusRef responseBodyRef delayRef= do liftIO $ print ("Request to be change mock response received")
                                                      uriStatus <- extractUriParam "id"
                                                      delayResponse <- extractUriParam "delay"
                                                      requestBody <- getBodyParam
                                                      liftIO (writeIORef statusRef uriStatus)
                                                      liftIO (writeIORef responseBodyRef requestBody)
                                                      liftIO (writeIORef delayRef (delayResponse * 1000)) -- ThreadDelay works in nano seconds
                                                      Web.Scotty.status status200 >> text "Mock server change successfully"

{-| Function to get uriParams from the uri request-}
extractUriParam :: Text -> ActionM Int
extractUriParam uriParam = Web.Scotty.param uriParam

{-| In scotty we have [body] operator to get the request body.
    We also use [decode] operator to extract and transform from json to Maybe of type we specify in the type signature-}
getBodyParam :: ActionT Text IO String
getBodyParam = do requestBody <- body
                  body <- (liftAndCatchIO $ return requestBody)
                  return $ unpack body

{-| Function to retrieve an Int as http status code and we transform into Scotty [statusXXX]-}
transformStatusCodeToStatus::Int -> Status
transformStatusCodeToStatus status =  case status of
                                       100 -> status100
                                       101 -> status101
                                       200 -> status200
                                       201 -> status201
                                       202 -> status202
                                       300 -> status300
                                       301 -> status301
                                       302 -> status302
                                       400 -> status400
                                       401 -> status401
                                       402 -> status402
                                       403 -> status403
                                       404 -> status404
                                       500 -> status500
                                       501 -> status501
                                       502 -> status502
                                       503 -> status503
                                       _ -> status511

