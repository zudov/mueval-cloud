{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Types where

import           Data.Text (Text)
import           Data.Aeson
import           GHC.Generics


data EvalRequest = EvalRequest { code :: Text
                               , tests :: [Text]
                               , restricted :: [Text]
                               } deriving (Show, Generic)

data EvalResponse = BadCode Text 
                  | Results [TestResult] 
                  deriving (Show, Generic)

data TestResult = TestPassed
                | TestFailed { lhsExpr :: Text
                             , lhsResult :: Text
                             }
                | EvaluationFailed Text deriving (Show, Generic)

instance FromJSON EvalRequest
instance ToJSON EvalRequest

instance ToJSON TestResult where
    toJSON (TestPassed) = object ["status" .= ("test-passed" :: Text)]
    toJSON (TestFailed expr result) = 
        object [ "status" .= ("test-failed" :: Text)
               , "lhs-expr" .= expr
               , "lhs-result" .= result]
    toJSON (EvaluationFailed err) =
        object [ "status" .= ("evaluation-failed" :: Text)
               , "error-msg" .= err]

instance ToJSON EvalResponse where
    toJSON (BadCode desc) = object [ "status" .= ("bad-code" :: Text)
                                   , "problem-desc" .= desc]
    toJSON (Results res) = object [ "status" .= ("success" :: Text)
                                  , "results" .= res]
