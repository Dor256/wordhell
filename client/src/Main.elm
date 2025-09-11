module Main exposing (..)

import Array exposing (..)
import Browser
import Css exposing (..)
import Elements exposing (..)
import Set exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick)
import Keyboard exposing (KeyAction(..), onKeyPress)
import LocalStorage
import Keyboard exposing (translateKey)
import Task
import Process
import Storage exposing (guessesStorageKey, dailyWordStorageKey, storageHandlers)
import Model exposing (Model, Msg(..), Guess, Outcome(..), Score(..), scoreGuess, updateLetterScore)
import Dict

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { guesses = []
      , word = ""
      , currentGuess = ""
      , gameOutcome = Nothing
      , guessedLetters = Dict.empty
      }
    , Cmd.batch
        [ runCmd (OnLoad guessesStorageKey)
        , runCmd (OnLoad dailyWordStorageKey)
        ]
    )


wordSize : Int
wordSize = 5

maxGuesses : Int
maxGuesses = 6

trimGuess : Guess -> Guess
trimGuess = String.left wordSize

runCmd : msg -> Cmd msg
runCmd msg =
    Task.succeed msg
        |> Task.perform identity

runDelayedCmd : Float -> msg -> Cmd msg
runDelayedCmd delay msg =
    delay
        |> Process.sleep
        |> Task.perform (always msg)

performDelayedMessage : msg -> Cmd msg
performDelayedMessage =
    runDelayedCmd (transitionDelayMs * 6)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchWord (Ok res) ->
            ( { model | word = res }, runCmd (Save ( dailyWordStorageKey, res )) )

        FetchWord (Err _) ->
            ( model, Cmd.none )

        Save ( key, value ) ->
            ( model, LocalStorage.saveString ( key, value ) )

        OnLoad key ->
            ( model, LocalStorage.onLoad key )

        Load (key, maybeValue) ->
            case Dict.get key storageHandlers of
                Just handler ->
                    handler maybeValue model

                Nothing ->
                    ( model, Cmd.none )

        TypeLetter key ->
            case key of
                Letter letter ->
                    ( { model | currentGuess = trimGuess (model.currentGuess ++ String.fromChar letter) }
                    , Cmd.none
                    )

                Submit ->
                    if String.length model.currentGuess == wordSize then
                        let
                            newGuesses = model.guesses ++ [model.currentGuess]
                            scoredDict = updateLetterScore { model | guesses = newGuesses }
                            isCorrectGuess = model.currentGuess == model.word
                            cmd = if isCorrectGuess then
                                    Cmd.batch
                                        [ performDelayedMessage (GameOver Win)
                                        , LocalStorage.clear ()
                                        ]
                                  else if List.length model.guesses + 1 >= maxGuesses then
                                    Cmd.batch
                                        [ performDelayedMessage (GameOver Lose)
                                        , LocalStorage.clear ()
                                        ]
                                  else
                                    Cmd.batch
                                        [ runCmd (Save (guessesStorageKey, String.join "," newGuesses))
                                        , runDelayedCmd (transitionDelayMs * 5) (UpdateKeyboard scoredDict)
                                        ]
                        in
                        ( { model
                            | currentGuess = ""
                            , guesses = List.take maxGuesses newGuesses
                          }
                        , cmd
                        )
                    else ( model, Cmd.none )

                Delete ->
                    ( { model | currentGuess = String.dropRight 1 model.currentGuess }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )
        UpdateKeyboard scoredKeys ->
            ( { model | guessedLetters = scoredKeys }, Cmd.none )
        GameOver outcome ->
            ( { model | gameOutcome = Just outcome }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ LocalStorage.loadString Load
        , case model.gameOutcome of
            Just _ ->
                Sub.none
            Nothing ->
                Sub.map TypeLetter onKeyPress
        ]

emptyRow : Html msg
emptyRow =
    rowContainer [] (List.repeat wordSize emptyTile)

emptyTile : Html msg
emptyTile =
    tile [] []

renderCurrentGuess : Guess -> Html msg
renderCurrentGuess guess =
    let
        toTile : Char -> Html msg
        toTile letter = tile [] [text <| String.fromChar letter]
        trimmedGuess = trimGuess guess
        padding = wordSize - String.length trimmedGuess
    in
    rowContainer [] <|
        (trimmedGuess
            |> String.toList
            |> List.map toTile
        ) ++ List.repeat padding emptyTile

renderScoredGuess : String -> Guess -> Html msg
renderScoredGuess word guess =
    let
        trimmedGuess = trimGuess guess
        scoredGuess : List (Char, Score)
        scoredGuess = scoreGuess word trimmedGuess
        fixedGuess = removeMisplacedIfHit word scoredGuess
    in
    rowContainer [] <|
        List.indexedMap
            (\index (letter, score) ->
                case score of
                    Hit ->
                        hitTile index [] [text <| String.fromChar letter]
                    Misplaced ->
                        misplacedTile index [] [text <| String.fromChar letter]
                    Miss ->
                        missTile index [] [text <| String.fromChar letter]
            )
            fixedGuess

removeMisplacedIfHit : String -> List ( Char, Score ) -> List ( Char, Score )
removeMisplacedIfHit word scoredGuess =
    let
        numberOfHits : Char -> Int
        numberOfHits letter =
            List.length <| List.filter ((==)(letter, Hit)) scoredGuess

        numberOfLettersInWord : Char -> Int
        numberOfLettersInWord letter =
            String.length <| String.filter ((==) letter) word

        shouldReplaceMisplaced : ( Char, Score ) -> Bool
        shouldReplaceMisplaced (letter, score) =
            List.member (letter, Hit) scoredGuess && score == Misplaced && (numberOfHits letter >= numberOfLettersInWord letter)
    in
    List.map
        (\(letter, score) ->
            if shouldReplaceMisplaced (letter, score) then
                (letter, Miss)
            else
                (letter, score)
        )
        scoredGuess

wordleGrid : Model -> Html msg
wordleGrid { guesses, currentGuess, word } =
    let
        unusedGuesses =
           max 0
                <| maxGuesses - List.length guesses - (if String.isEmpty currentGuess then 0 else 1)
    in
    gridContainer [] <|
        List.concat
            [ List.map (renderScoredGuess word) guesses
            , if String.isEmpty currentGuess || List.length guesses >= maxGuesses then [] else [ renderCurrentGuess currentGuess ]
            , List.repeat unusedGuesses emptyRow
            ]

toKeys : String -> Model -> List (Html Msg)
toKeys keys model =
    let
        coloredDict = Dict.map (\_ score ->
            case score of
                Hit -> hex "#538d4e"
                Misplaced -> hex "#b59f3b"
                Miss -> hex "#3a3a3c"
            ) model.guessedLetters        
        scoreToBackgroundColor letter =
            coloredDict
                |> Dict.get letter
                |> Maybe.map (\color -> backgroundColor color)
                |> Maybe.withDefault (backgroundColor (hex "#818384"))
        toKey letter =
            styled key [scoreToBackgroundColor letter]
                [ onClick <| TypeLetter (translateKey letter) ]
                [ text <| String.fromChar letter ]
    in    
    keys
        |> String.toList
        |> List.map toKey

keyboard : Model -> Html Msg
keyboard model =
    keyboardContainer []
        [ keyRow [] <| toKeys "qwertyuiop" model
        , keyRow [] <|
            List.concat 
                [ [spacer [] []]
                , toKeys "asdfghjkl" model
                , [spacer [] []]
                ]
        , keyRow [] <| 
            List.concat
                [ [ enterKey [onClick <| TypeLetter Submit] [text "Enter"] ]
                , toKeys "zxcvbnm" model
                , [ deleteKey [onClick <| TypeLetter Delete] [text "⌫"] ]
                ]
        ]

gameOverModal : Maybe Outcome -> Html msg
gameOverModal maybeOutcome =
    case maybeOutcome of
        Just outcome ->
            modal True 
               [
                    case outcome of
                        Win ->
                            text "You Won!"
                        Lose ->
                            text "Game Over!"
               ]
        Nothing ->
            modal False []

view : Model -> Html Msg
view model =
    page []
        [ container []
            [ heading [] [ text "Wordhell" ]
            , text model.word
            , gameOverModal model.gameOutcome
            , wordleGrid model
            , keyboard model
            ]
        ]
  