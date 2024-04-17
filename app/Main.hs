module Main where

import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import WordList (
  Filepath (Filepath),
  getFilepathFromEnv,
  getWordList,
 )

main :: IO ()
main = do
  filepath <- getFilepathFromEnv
  contents <- runReaderT getWordList $ Filepath filepath
  print contents