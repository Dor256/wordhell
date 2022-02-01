module Main exposing (..)

import Browser
import Api exposing (fetchWord, WordResponse)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Css exposing (..)
import LocalStorage
import Elements exposing (..)

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
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( { guesses = []
      , word = ""
      }
    , fetchWord FetchWord
    )

type Msg 
    = FetchWord WordResponse
    | Save
    | OnLoad String
    | Load (Maybe String)

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        FetchWord (Ok res) ->
            ({ model | word = res }, Cmd.none)

        FetchWord (Err _) ->
            (model, Cmd.none)
        
        Save ->
            (model, LocalStorage.saveString ("key", "value"))

        OnLoad key ->
            (model, LocalStorage.onLoad key)
    
        Load maybeValue ->
            case maybeValue of
                Just value ->
                    ({ model | guesses = model.guesses ++ [value] }, Cmd.none)
                
                Nothing ->
                    (model, Cmd.none)
            


subscriptions : Model -> Sub Msg
subscriptions _ =
    LocalStorage.loadString Load

view : Model -> Html Msg
view model =
    page []
        [ container [ onClick Save ]
            [ h2 [] [ text "Wordhell" ]
            , text model.word
            , text "load"
            , div [ css [ displayFlex, width (pct 100), height (pct 100), justifyContent center ] ]
                [ letter [] []
                , letter [] []
                , letter [] []
                , letter [] []
                , letter [] []
                ]
            ]
        ]
    
        