module Main exposing (..)

import Api exposing (WordResponse, fetchWord)
import Array exposing (..)
import Browser
import Css exposing (..)
import Elements exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onClick)
import Keyboard exposing (KeyAction(..), onKeyPress)
import LocalStorage


type alias Guess =
    Array (Maybe Char)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }


type alias Model =
    { guesses : List (Maybe String)
    , word : String
    , currentTile : Int
    , currentGuess : Guess
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { guesses = [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
      , word = ""
      , currentTile = 0
      , currentGuess = Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing ]
      }
    , fetchWord FetchWord
    )


type Msg
    = FetchWord WordResponse
    | Save
    | OnLoad String
    | Load (Maybe String)
    | TypeLetter KeyAction


incrementCurrentTile : Int -> Int
incrementCurrentTile tile =
    if tile < 5 then
        tile + 1

    else
        tile


decrementCurrentTile : Int -> Int
decrementCurrentTile tile =
    if tile > 0 then
        tile - 1

    else
        tile


applyCurrentLetter : Model -> Char -> Guess
applyCurrentLetter { currentTile, currentGuess } letter =
    Array.set currentTile (Just letter) currentGuess


deleteCurrentLetter : Model -> Guess
deleteCurrentLetter { currentTile, currentGuess } =
    Array.set (currentTile - 1) Nothing currentGuess


update : Msg -> Model -> ( Model, Cmd msg )
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
                    ( { model | guesses = model.guesses ++ [ Just value ] }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        TypeLetter key ->
            case key of
                Letter letter ->
                    ( { model
                        | currentTile = incrementCurrentTile model.currentTile
                        , currentGuess = applyCurrentLetter model letter
                      }
                    , Cmd.none
                    )

                Submit ->
                    ( model, Cmd.none )

                Delete ->
                    ( { model
                        | currentTile = decrementCurrentTile model.currentTile
                        , currentGuess = deleteCurrentLetter model
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ LocalStorage.loadString Load
        , Sub.map TypeLetter onKeyPress
        ]


wordRows : Guess -> Html msg
wordRows guess =
    let
        renderTile maybeLetter =
            case maybeLetter of
                Just letter ->
                    tile [] [ text <| String.fromChar letter ]

                Nothing ->
                    tile [] []
    in
    Array.map renderTile guess |> Array.toList |> rowContainer []


view : Model -> Html Msg
view model =
    page []
        [ container [ onClick Save ]
            [ heading [] [ text "Wordhell" ]
            , text model.word
            , wordRows model.currentGuess |> List.repeat 6 |> gridContainer []
            ]
        ]
