module Model exposing (..)

import Api exposing (WordResponse)
import Keyboard exposing (KeyAction)
import Dict exposing (Dict)
import Set

type alias Guess = String

type Outcome = Win | Lose

type Score
    = Hit
    | Miss
    | Misplaced

type alias Model =
    { guesses : List String
    , word : String
    , currentGuess : Guess
    , gameOutcome : Maybe Outcome
    , guessedLetters : Dict Char Score
    }

type Msg
    = FetchWord WordResponse
    | Save ( String, String )
    | OnLoad String
    | Load ( String, Maybe String )
    | TypeLetter KeyAction
    | GameOver Outcome
    | UpdateKeyboard (Dict Char Score)


scoreGuess : String -> Guess -> List ( Char, Score )
scoreGuess word guess =
    let
        wordChars = String.toList word
        wordSet = Set.fromList wordChars
        guessChars = String.toList guess
    in
    List.map2
        (\wordChar guessChar ->
            ( guessChar
            , if guessChar == wordChar then
                Hit
              else if wordSet |> Set.member guessChar then
                Misplaced
              else
                Miss
            )
        )
        wordChars
        guessChars


updateLetterScore : Model -> Dict.Dict Char Score
updateLetterScore model =
    let
        scoredLetters = List.concatMap (scoreGuess model.word) model.guesses
    in
    List.foldl
        (\(char, score) dict ->
            case Dict.get char dict of
                Just Hit ->
                    dict
                Just Misplaced ->
                    if score == Hit then Dict.insert char score dict else dict
                _ ->
                    Dict.insert char score dict
        )
    model.guessedLetters
    scoredLetters
