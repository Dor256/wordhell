module Model exposing (..)

import Api exposing (WordResponse)
import Keyboard exposing (KeyAction)

type alias Guess = String

type Outcome = Win | Lose

type alias Model =
    { guesses : List String
    , word : String
    , currentGuess : Guess
    , gameOutcome : Maybe Outcome
    }

type Msg
    = FetchWord WordResponse
    | Save ( String, String )
    | OnLoad String
    | Load ( String, Maybe String )
    | TypeLetter KeyAction
    | GameOver Outcome
