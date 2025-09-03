module Storage exposing (guessesStorageKey, dailyWordStorageKey, storageHandlers)
import Model exposing (Model, Msg)
import Model exposing (Msg(..))
import Api exposing (fetchWord)
import Dict exposing (Dict)

type alias StorageHandler =
  Maybe String -> Model -> ( Model, Cmd Msg )

guessesStorageKey : String
guessesStorageKey = "guesses"

dailyWordStorageKey : String
dailyWordStorageKey = "dailyWord"

updateGuesses : StorageHandler
updateGuesses maybeValue model =
    let
        newModel = Maybe.withDefault model <|
            Maybe.map (\value -> { model
                | guesses = model.guesses ++ String.split "," value
                , currentGuess = ""
              }) maybeValue
    in
    ( newModel, Cmd.none )

updateDailyWord : StorageHandler
updateDailyWord maybeValue model =
    case maybeValue of
        Just value ->
            ( { model | word = value }, Cmd.none )

        Nothing ->
            ( model, fetchWord FetchWord )

storageHandlers : Dict String StorageHandler
storageHandlers =
    Dict.fromList
        [ ( guessesStorageKey, updateGuesses )
        , ( dailyWordStorageKey, updateDailyWord )
        ]
