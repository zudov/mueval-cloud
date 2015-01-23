{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell #-}
module Util where

import Data.Pool (withResource, Pool, createPool)
import Network.AMQP
import System.Log.Logger
import Data.Text (Text)

import HFlags

defineFlag "rabbit_host" ("localhost" :: String) "RabbitMQ host"
defineFlag "rabbit_user" ("guest" :: Text) "RabbitMQ user"
defineFlag "rabbit_pass" ("guest" :: Text) "RabbitMQ pass"
defineFlag "rabbit_virt_host" ("/" :: Text) "RabbitMQ virtual host"

data AMQPResource = AMQPResource
    { connection      :: Connection
    , msgChannel      :: Channel -- ^ Channel for sending messages
    , consumerChannel :: Channel -- ^ Channel used by consumer
    , consumerTag     :: Maybe ConsumerTag
    }

send :: Pool AMQPResource -> Text -> Message -> IO ()
send amqp queue msg = withResource amqp $ \res -> 
                        publishMsg (msgChannel res) "" queue msg

-- | Creates a pool with two channels
amqpPool :: String -- ^ Name of the logger to use
         -> (AMQPResource -> IO AMQPResource) -- ^ action to run after the initialization
         -> IO (Pool AMQPResource)
amqpPool logger afterInit = createPool create destroy 1 36000 2
    where create = do
              infoM logger "Creating a new AMQP resource"
              connection <- openConnection flags_rabbit_host flags_rabbit_virt_host 
                                           flags_rabbit_user flags_rabbit_pass
              consumerChannel <- openChannel connection
              msgChannel <- openChannel connection
              afterInit (AMQPResource{consumerTag = Nothing, ..})

          destroy AMQPResource{..} = do
              infoM logger "Destroying AMQP resource"
              maybe (return ()) (cancelConsumer consumerChannel) consumerTag
              closeConnection connection
