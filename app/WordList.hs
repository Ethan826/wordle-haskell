{-# LANGUAGE DeriveGeneric #-}

module WordList (
  Filepath (..),
  WordList (..),
  CandidateWord (..),
  ParseJsonError (..),
  getWordList,
  getFilepathFromEnv,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import Text.Printf (printf)

{- | Retrieves the filepath from the environment variable `WORDLE_LIST_FILENAME`.

This function looks up the value of the environment variable
`WORDLE_LIST_FILENAME` and returns it as a `String`. If the environment variable
is not set, it throws an 'IOError' with a descriptive error message.
-}
getFilepathFromEnv :: IO String
getFilepathFromEnv = do
  maybeF <- lookupEnv filenameEnvVar
  case maybeF of
    Just f -> pure f
    Nothing -> throwIO envVarNotSetError
 where
  filenameEnvVar = "WORDLE_LIST_FILENAME"
  envVarNotSetError =
    userError $
      printf "`%s` env var not set" filenameEnvVar

------------------------------------------------------------
-- getWordList (main export)
------------------------------------------------------------

getWordList :: ReaderT Filepath IO (Either ParseJsonError WordList)
getWordList = parseWordList <$> readWordListFromFile

newtype Filepath = Filepath {getFilepath :: String}

newtype WordList = WordList [CandidateWord]
  deriving (Generic, Show, Eq)

instance FromJSON WordList

newtype CandidateWord = CandidateWord Text
  deriving (Generic, Show, Eq)

instance FromJSON CandidateWord

------------------------------------------------------------
-- readWordListFromFile and related types
------------------------------------------------------------

readWordListFromFile :: ReaderT Filepath IO BL.ByteString
readWordListFromFile = asks getFilepath >>= liftIO . BL.readFile

------------------------------------------------------------
-- parseWordList and related types
------------------------------------------------------------

parseWordList :: BL.ByteString -> Either ParseJsonError WordList
parseWordList = first ParseJsonError . eitherDecode

newtype ParseJsonError = ParseJsonError String
  deriving (Show)

instance Exception ParseJsonError