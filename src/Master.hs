{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import           Data.Text (Text, pack)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Concurrent.Map as CM
import           Control.Monad.IO.Class (liftIO)

import           Network.AMQP

import           System.Log.Logger

import           Web.Scotty

import           Config
import           Util
import           HFlags

import System.Log.Formatter
import System.Log.Handler.Simple (streamHandler)
import System.Log.Handler (setFormatter)
import System.IO (stderr)

defineEQFlag "loglevel" [| INFO :: Priority |] "Priority" 
             "Logging priority: DEBUG|INFO|NOTICE|WARNING|ERROR|CRITICAL|ALERT|EMERGENCY"
defineFlag "rest_port" (4000 :: Int) "Port for running master's rest interface"

$(return [])

main :: IO ()
main = do
    _ <- $initHFlags "Distributed mueval based haskell evaluation"
    stderrHandler <- streamHandler stderr flags_loglevel
    updateGlobalLogger rootLoggerName (setLevel flags_loglevel)
    updateGlobalLogger rootLoggerName 
        (setHandlers [setFormatter stderrHandler (simpleLogFormatter 
                                                  "[$time : $prio : $loggername] $msg")])
    master

-- As all work is done on workers, master only needs to route the REST requests
-- to them through AMQP, gathering the workers' replies and routing these replies
-- back as REST responses.
--
-- In order to acomplish that master launches two threads:
-- 1. REST interface (runs in the main thread)
--    Listens to REST requests. When receives one it generates and UUID uses
--    it as a key in the responseMap with empty MVar as a value. It then sends
--    a message into the AMQP queue, the message contains the tests that came
--    with REST requests and the UUID and msgCorrelationID. After that it blocks
--    on the created MVar, until the responseConsumer wouldn't put the results
--    in there. After that it simply reply with results to the REST client.
-- 2. AMQP consumer (responseConsumer).
--    Consumes the messages from the AMQP queue, when message is received it
--    takes its correlationID and looks up the corresponding MVar from respsMap,
--    when it has MVar it simply puts the received results in there, so that
--    they can be send to the REST client.
--
--    If the connection gets broken it would be implicitly reestablished when
--    there would be made an attempt to send a message (see Utils module).

master :: IO ()
master = do
    infoM "master.started" "Master started"
    respsMap <- CM.empty
    amqp <- amqpPool "master.pool" $ \res@AMQPResource{..} -> do
        declareQueue msgChannel newQueue {queueName = requestQueue}
        declareQueue consumerChannel newQueue {queueName = respondQueue}
        tag <- consumeMsgs consumerChannel respondQueue NoAck $ responseConsumer respsMap
        return res {consumerTag = Just tag}

    infoM "master.rest.launch" "Launching the REST interface"
    scotty flags_rest_port $ do 
        post "/eval" $ do
            request <- body
            result <- liftIO $ do
                infoM "master.rest.gotRequest" $ show request
                corrID <- pack <$> UUID.toString <$> UUID.nextRandom
                respVar <- newEmptyMVar
                CM.insert corrID respVar respsMap
                infoM "master.rest.request" "Pushing task"
                send amqp requestQueue 
                     newMsg { msgCorrelationID = Just corrID
                            , msgContentType = Just "application/json"
                            , msgReplyTo = Just respondQueue
                            , msgBody = request
                            }
                takeMVar respVar
            raw result

responseConsumer :: CM.Map Text (MVar BL.ByteString) -> (Message, Envelope) -> IO ()
responseConsumer respsMap (Message{..},env) 
    | Nothing <- msgCorrelationID =
        errorM "master.responseConsumer" 
               "Received message lacks correlation ID"
    | Just uuid <- msgCorrelationID = do
        mRespVar <- CM.lookup uuid respsMap
        case mRespVar of
            Nothing -> errorM "master.callback" "The respMap lacks corresponding uuid"
            Just respVar -> do putMVar respVar msgBody
                               CM.delete uuid respsMap
