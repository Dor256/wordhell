port module LocalStorage exposing (..)

port saveString : ( String, String ) -> Cmd msg

port onLoad : String -> Cmd msg

port loadString : (Maybe String -> msg) -> Sub msg

port deleteItem : String -> Cmd msg
