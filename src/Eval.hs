{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Eval where

import           System.Exit (ExitCode(..))
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T (lines, unlines, unpack)
import           System.Process.Text (readProcessWithExitCode)

data Evaluate = Evaluate { expression :: Text
                         , loadFile   :: FilePath
                         , timeout    :: Int
                         } deriving (Show)

-- TODO: Use single temp dir per worker process
mueval :: Evaluate -> IO (Either Text Text)
mueval (Evaluate {..}) = do
    let options = [ "-S", "-i", "-n", "-l", loadFile
                  , "-t", show timeout, "-e", T.unpack expression]
    (status,out,err) <- readProcessWithExitCode "mueval" options ""
    return $ readMueval status out err

readMueval :: ExitCode -> Text -> Text -> (Either Text Text)
readMueval ExitSuccess out _
    | [_,_,value] <- T.lines out = Right value
    | otherwise = Left "Unable to get type and value"
readMueval (ExitFailure errorCode) out err = Left $ case errorCode of
    1 | err == "mueval-core: Time limit exceeded\n" -> "Time limit exceeded"
    1 -> T.unlines $ T.lines out
    -9 -> "Time limit exceeded"
    _  -> out <> "\n" <> err
