{-# LANGUAGE OverloadedStrings #-}
module Config where

import           Data.Text (Text)

requestQueue, respondQueue :: Text
requestQueue = "requestQueue"
respondQueue = "respondQueue"
