{-# LANGUAGE DeriveGeneric #-}

{- | Module for handling word lists.

This module provides functionality for reading and parsing word lists from
files. It defines types for representing filepaths, word lists, candidate words,
and parsing errors.
-}
module WordList (
  Filepath (..),
  WordList (..),
  CandidateWord (..),
  ParseJsonError (..),
  getWordList,
) where

import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import GHC.Generics (Generic)

{- | Retrieves the word list from a file.

This function reads the word list from a file specified by the `Filepath` using
`readWordListFromFile` and parses it using `parseWordList`. The result is
returned as an `Either` value, where `Left` contains a `ParseJsonError` if
parsing fails, and `Right` contains the successfully parsed `WordList`.
-}
getWordList :: ReaderT Filepath IO (Either ParseJsonError WordList)
getWordList = parseWordList <$> readWordListFromFile

{- | Represents a filepath.

The `Filepath` type wraps a `String` and provides a constructor for creating
filepaths.
-}
newtype Filepath = Filepath {getFilepath :: String}

{- | Represents a word list.

The `WordList` type wraps a list of `CandidateWord`s and provides a constructor
for creating word lists.
-}
newtype WordList = WordList [CandidateWord]
  deriving (Generic, Show, Eq)

instance FromJSON WordList

{- | Represents a candidate word.

The `CandidateWord` type wraps a `Text` value and provides a constructor for
creating candidate words.
-}
newtype CandidateWord = CandidateWord Text
  deriving (Generic, Show, Eq)

instance FromJSON CandidateWord

{- | Reads the word list from a file.

This function takes a `Filepath` and reads the contents of the file as a lazy
`ByteString`.  It uses `asks` to retrieve the filepath from the `ReaderT`
environment and `liftIO` to perform the file reading operation in the `IO`
monad.
-}
readWordListFromFile :: ReaderT Filepath IO BL.ByteString
readWordListFromFile = asks getFilepath >>= liftIO . BL.readFile

{- | Parses the word list from a `ByteString`.

This function takes a lazy `ByteString` representing the JSON-encoded word list
and attempts to parse it using `eitherDecode`. If parsing succeeds, it returns
the parsed `WordList` wrapped in `Right`. If parsing fails, it returns a
`ParseJsonError` wrapped in `Left`.
-}
parseWordList :: BL.ByteString -> Either ParseJsonError WordList
parseWordList = first ParseJsonError . eitherDecode

{- | Represents a JSON parsing error.

The `ParseJsonError` type wraps a `String` error message and provides a
constructor for creating parsing errors.
-}
newtype ParseJsonError = ParseJsonError String
  deriving (Show)

instance Exception ParseJsonError