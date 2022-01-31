module Main exposing (..)

import Browser
import Api exposing (fetchWord, WordResponse)
import Html exposing (Html, div, text)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

type alias Model =
    { guesses : List String
    , word : String
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( { guesses = []
      , word = ""
      }
    , fetchWord FetchWord
    )

type Msg =
    FetchWord WordResponse

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        FetchWord (Ok res) ->
            ({ model | word = res }, Cmd.none) 
        FetchWord (Err _) ->
            (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Model -> Html Msg
view _ =
    div [] [text "Hello"]
        