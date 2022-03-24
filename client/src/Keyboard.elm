module Keyboard exposing (KeyAction(..), onKeyPress, translateKey)

import Browser.Events exposing (onKeyDown)
import Json.Decode as Json


type KeyAction
    = Letter Char
    | Submit
    | Delete
    | NoOp


translateKey : Char -> KeyAction
translateKey key =
    case key of
        'q' ->
            Letter 'q'

        'w' ->
            Letter 'w'

        'e' ->
            Letter 'e'

        'r' ->
            Letter 'r'

        't' ->
            Letter 't'

        'y' ->
            Letter 'y'

        'u' ->
            Letter 'u'

        'i' ->
            Letter 'i'

        'o' ->
            Letter 'o'

        'p' ->
            Letter 'p'

        'a' ->
            Letter 'a'

        's' ->
            Letter 's'

        'd' ->
            Letter 'd'

        'f' ->
            Letter 'f'

        'g' ->
            Letter 'g'

        'h' ->
            Letter 'h'

        'j' ->
            Letter 'j'

        'k' ->
            Letter 'k'

        'l' ->
            Letter 'l'

        'z' ->
            Letter 'z'

        'x' ->
            Letter 'x'

        'c' ->
            Letter 'c'

        'v' ->
            Letter 'v'

        'b' ->
            Letter 'b'

        'n' ->
            Letter 'n'

        'm' ->
            Letter 'm'

        _ ->
            NoOp


keyDecoder : Json.Decoder KeyAction
keyDecoder =
    Json.map toKey (Json.field "key" Json.string)


toKey : String -> KeyAction
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            translateKey char

        _ ->
            case keyValue of
                "Enter" ->
                    Submit

                "Backspace" ->
                    Delete

                _ ->
                    NoOp


onKeyPress : Sub KeyAction
onKeyPress =
    onKeyDown keyDecoder
