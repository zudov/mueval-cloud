{-# LANGUAGE OverloadedStrings #-}
-- This module is for working with the user submitted code
module Code (validateCode, makeFileContent) where

import           Text.Regex.Posix
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

validateCode :: Text -> Either Text Text
validateCode code
    | T.null code = Left "Empty input is not allowed"
    | T.unpack code =~ ("(^| |\n|\t)import( |\n|\t)" :: String) = Left "Imports are not allowed"
    | otherwise = Right code

makeFileContent :: Text -> [Text] -> [Text] -> Text
makeFileContent code tests restricted =
    mconcat [ "module Input where\n"
            , "import Prelude hiding (", T.intercalate "," restricted, ")", "\n"
            , addBlankDef code tests]

addBlankDef :: Text -> [Text] -> Text
addBlankDef code tests
    | T.unpack code =~ ("__" :: String) = code
    | not (any (\expr -> T.unpack expr =~ ("__" :: String)) tests) = code
    | otherwise     = "__ = " <> code

