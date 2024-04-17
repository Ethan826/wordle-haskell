module Main where

import Control.Exception (throwIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import System.Environment (lookupEnv)
import Text.Printf (printf)
import WordList (
  Filepath (Filepath),
  getWordList,
 )

main :: IO ()
main = do
  filepath <- getFilepathFromEnv
  contents <- runReaderT getWordList filepath
  print contents

{- | Retrieves the filepath from the environment variable `WORDLE_LIST_FILENAME`.

This function looks up the value of the environment variable
`WORDLE_LIST_FILENAME` and returns it as a `String`. If the environment variable
is not set, it throws an 'IOError' with a descriptive error message.
-}
getFilepathFromEnv :: IO Filepath
getFilepathFromEnv = do
  maybeF <- lookupEnv filenameEnvVar
  case maybeF of
    Just f -> pure $ Filepath f
    Nothing -> throwIO envVarNotSetError
 where
  filenameEnvVar = "WORDLE_LIST_FILENAME"
  envVarNotSetError =
    userError $
      printf "`%s` env var not set" filenameEnvVar