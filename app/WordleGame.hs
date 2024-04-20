-- | Module for representing and manipulating Wordle game elements.
module WordleGame where

import Data.Char (isLower)
import Text.Printf (printf)

{- | Represents a letter in a Wordle game.

A `WordleLetter` is a newtype wrapper around a `Char` that ensures the character
is a lowercase letter. It provides a constructor `WordleLetter` and an accessor
`getWordleLetter` for retrieving the underlying character.
-}
newtype WordleLetter = WordleLetter {getWordleLetter :: Char}
  deriving (Show, Eq, Ord)

{- | Smart constructor for creating a `WordleLetter`.

This function takes a `Char` and returns either a valid `WordleLetter` wrapped
in `Right` or an error message wrapped in `Left`. It validates that the input
character is a lowercase letter.
-}
mkWordleLetter :: Char -> Either String WordleLetter
mkWordleLetter c =
  if isLower c
    then Right $ WordleLetter c
    else
      Left $
        printf "invalid Wordle letter "
          <> show c
          <> ". Must be a lowercase letter"

{- | Represents the result of guessing a letter in a Wordle game.

A `WordleLetterGuessResult` can be one of three constructors:
- `Green` indicates that the letter is correct and in the right position.
- `Yellow` indicates that the letter is correct but in the wrong position.
- `Gray` indicates that the letter is not present in the word.
-}
data WordleLetterGuessResult
  = Green WordleLetter
  | Yellow WordleLetter
  | Gray WordleLetter
  deriving (Show, Eq)

{- | Represents a guess in a Wordle game.

A `WordleGuess` consists of five `WordleLetterGuessResult`s, each representing
the result of guessing a letter in the word.
-}
data WordleGuess
  = WordleGuess
      WordleLetterGuessResult
      WordleLetterGuessResult
      WordleLetterGuessResult
      WordleLetterGuessResult
      WordleLetterGuessResult
  deriving (Show, Eq)

{- | Represents the state of a Wordle game.

A `WordleGame` can be in one of seven states, each representing a different stage of the game:

`InitialGame` represents the initial state of the game with no guesses made.
Each subsequent variant represents the game after the specified number of
guesses.
-}
data WordleGame
  = InitialGame
  | GuessOne WordleGuess
  | GuessTwo WordleGuess WordleGuess
  | GuessThree WordleGuess WordleGuess WordleGuess
  | GuessFour WordleGuess WordleGuess WordleGuess WordleGuess
  | GuessFive WordleGuess WordleGuess WordleGuess WordleGuess WordleGuess
  | GuessSix WordleGuess WordleGuess WordleGuess WordleGuess WordleGuess WordleGuess

data WordleAnswer
  = WordleAnswer
      WordleLetter
      WordleLetter
      WordleLetter
      WordleLetter
      WordleLetter
  deriving (Show, Eq)

-- | Represents the status of a Wordle game.
data WordleGameStatus
  = InProgress
  | Won
  | Lost
  deriving (Show, Eq)
