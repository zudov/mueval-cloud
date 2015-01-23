{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import           Control.Applicative
import           Control.Exception (handle)
import           Control.Concurrent
import           Control.Monad

import           Data.Aeson (decode, encode)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Pool

import           Network.AMQP

import           System.Log.Logger
import           System.Unix.Directory

import           Types
import           Eval
import           Config
import           Util
import           Code
import qualified Data.Text.IO as TIO

import           System.FilePath ((</>))
import           System.Log.Formatter
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Handler (setFormatter)
import           System.IO (stderr)
import           HFlags
import           Safe

defineEQFlag "loglevel" [| INFO :: Priority |] "Priority" 
             "Logging priority: DEBUG|INFO|NOTICE|WARNING|ERROR|CRITICAL|ALERT|EMERGENCY"
$(return [])

main :: IO ()
main = do
    _ <- $initHFlags "Distributed mueval based haskell evaluation"
    stderrHandler <- streamHandler stderr flags_loglevel
    updateGlobalLogger rootLoggerName (setLevel flags_loglevel)
    updateGlobalLogger rootLoggerName 
        (setHandlers [setFormatter stderrHandler (simpleLogFormatter 
                                                  "[$time : $prio : $loggername] $msg")])
    worker

-- The worker has three threads:
-- 1. AMQP message consumer (evaluator)
--    Evaluator consumes the messages from AMQP queue and evaluates the
--    tests that message contains. It then dispatches the evaluation results
--    to the replier using interChan (normal Concurrent.Chan)
-- 2. AMQP message sender (replier)
--    Replier gets messages from interChan and then sends into AMQP queue.
-- 3. pinger (works in main thread)
--    Pinger simply simply pings the server within certain interval by setting
--    qos. If the connection is broken it implicitly get's reestablished using
--    resource-pool (see Utils module)

worker :: IO ()
worker = withTemporaryDirectory "tmpdir" $ \tmpdir -> do
    interChan <- newChan
    amqp <- amqpPool "worker.pool" $ \res@AMQPResource{..} -> do
        qos consumerChannel 0 2 False
        declareQueue consumerChannel newQueue {queueName = requestQueue}
        declareQueue msgChannel newQueue {queueName = respondQueue}
        tag <- consumeMsgs consumerChannel requestQueue Ack $ evaluator tmpdir interChan
        return (res {consumerTag = Just tag})
    infoM "worker.replier" "Launching replier"
    forkIO $ replier interChan amqp
    pinger amqp 0

pinger :: Pool AMQPResource -> Int -> IO ()
pinger amqp delay = do
    threadDelay delay
    infoM "worker.pinger" "Pinging"
    handle handler $ withResource amqp $ \res -> qos (msgChannel res) 0 0 False
    pinger amqp 30000000
    where handler :: AMQPException -> IO ()
          handler e = do errorM "worker.pinger.fail" (show e)
                         pinger amqp 30000000
                      
replier :: Chan Message -> Pool AMQPResource -> IO ()
replier interChan amqp = forever $ do
    infoM "worker.replier" "Reading from interChan"
    msg <- readChan interChan
    infoM "worker.replier" "Sending message"
    send amqp respondQueue msg
    
evaluator :: FilePath -> Chan Message -> (Message,Envelope) -> IO ()
evaluator tmpdir interChan (Message{..},env) = do
    infoM "worker.evaluator.TaskReceived" $ unpack msgBody
    resp <- case decode msgBody of
        Nothing -> do errorM "worker.evaluator.decodeError" $ unpack msgBody
                      return $ BadCode "Evaluator failed to parse the message"
        Just req -> runTests tmpdir req
    infoM "worker.evaluator.done" $ show resp
    writeChan interChan newMsg { msgBody = encode resp
                               , msgCorrelationID = msgCorrelationID}
    ackEnv env

runTests :: FilePath -> EvalRequest -> IO EvalResponse
runTests tmpdir (EvalRequest {..}) =
    case validateCode code of
        Left err -> return $ BadCode err
        Right code -> do
            loadFile <- createLoadFile tmpdir (makeFileContent code tests restricted)
            Results <$> mapM (runTest loadFile) tests 

runTest :: FilePath -> Text -> IO TestResult
runTest loadFile test = do
    res <- mueval (Evaluate test loadFile 10)
    case res of
        Left err -> return $ EvaluationFailed err
        Right "True" -> return TestPassed
        Right _ -> case mLhs of
            Nothing -> return $ TestFailed "No left-hand-side in test expression" ""
            Just lhs -> do
                lhsRes <- mueval (Evaluate lhs loadFile 10)
                case lhsRes of
                    Left err -> return $ TestFailed "Evaluation of left-hand-side failed" ""
                    Right val -> return $ TestFailed lhs val
            where mLhs = T.strip <$> headMay (T.splitOn "==" test)

createLoadFile :: FilePath -> Text -> IO FilePath
createLoadFile tmpdir content = do
    TIO.writeFile filename content
    return filename
    where filename = tmpdir </> "Input.hs"
