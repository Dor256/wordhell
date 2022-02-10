module Api exposing (..)

import Http


type alias WordResponse =
    Result Http.Error String


baseUrl : String
baseUrl =
    "http://localhost:3000"


fetchWord : (WordResponse -> msg) -> Cmd msg
fetchWord msg =
    Http.get
        { url = baseUrl ++ "/word"
        , expect = Http.expectString msg
        }
