port module LocalStorage exposing (..)

port saveString : ( String, String ) -> Cmd msg

port onLoad : String -> Cmd msg

port loadString : (( String, Maybe String ) -> msg) -> Sub msg

port clear : () -> Cmd msg
