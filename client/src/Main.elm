module Main exposing (..)

import Api exposing (WordResponse, fetchWord)
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

type alias Guess = String

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }


type alias Model =
    { guesses : List String
    , word : String
    , currentGuess : Guess
    , isGameOver : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { guesses = []
      , word = ""
      , currentGuess = ""
      , isGameOver = False
      }
    , fetchWord FetchWord
    )


type Msg
    = FetchWord WordResponse
    | Save
    | OnLoad String
    | Load (Maybe String)
    | TypeLetter KeyAction
    | Complete

wordSize : Int
wordSize = 5

maxGuesses : Int
maxGuesses = 6

trimGuess : Guess -> Guess
trimGuess = String.left wordSize


performMessage : msg -> Cmd msg
performMessage msg =
    transitionDelayMs * 6 |> Process.sleep |> Task.perform (always msg)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchWord (Ok res) ->
            ( { model | word = res }, Cmd.none )

        FetchWord (Err _) ->
            ( model, Cmd.none )

        Save ->
            ( model, LocalStorage.saveString ( "key", "value" ) )

        OnLoad key ->
            ( model, LocalStorage.onLoad key )

        Load maybeValue ->
            case maybeValue of
                Just value ->
                    ( { model
                        | guesses = model.guesses ++ [value]
                        , currentGuess = ""
                      }
                    , Cmd.none
                    )

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
                            isCorrectGuess = model.currentGuess == model.word 
                        in
                        ( { model
                            | currentGuess = ""
                            , guesses = List.take maxGuesses (model.guesses ++ [model.currentGuess])
                        }
                        , if isCorrectGuess then performMessage Complete else Cmd.none
                        )
                    else ( model, Cmd.none )

                Delete ->
                    ( { model | currentGuess = String.dropRight 1 model.currentGuess }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )
        Complete ->
            ( { model | isGameOver = True }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ LocalStorage.loadString Load
        , if model.isGameOver then
            Sub.none
          else
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

type Score
    = Hit
    | Miss
    | Misplaced

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

toKeys : String -> List (Html Msg)
toKeys keys =
    let
        toKey letter = key [ onClick <| TypeLetter (translateKey letter) ] [text <| String.fromChar letter]
    in    
    keys
        |> String.toList
        |> List.map toKey

keyboard : Html Msg
keyboard =
    keyboardContainer []
        [ keyRow [] <| toKeys "qwertyuiop"
        , keyRow [] <|
            List.concat 
                [ [spacer [] []]
                , toKeys "asdfghjkl"
                , [spacer [] []]
                ]
        , keyRow [] <| 
            List.concat
                [ [ enterKey [onClick <| TypeLetter Submit] [text "Enter"] ]
                , toKeys "zxcvbnm"
                , [ deleteKey [onClick <| TypeLetter Delete] [text "⌫"] ]
                ]
        ]


view : Model -> Html Msg
view model =
    page []
        [ container [ onClick Save ]
            [ heading [] [ text "Wordhell" ]
            , text model.word
            , modal model.isGameOver [ text "Game Over!" ]
            , wordleGrid model
            , keyboard
            ]
        ]
  