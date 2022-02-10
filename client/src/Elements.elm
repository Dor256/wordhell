module Elements exposing (container, gridContainer, heading, page, rowContainer, tile)

import Css exposing (..)
import Html.Styled exposing (Attribute, Html, div, h2, main_, styled)


type alias HtmlElement msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


gridGap : Float -> Style
gridGap pixels =
    property "grid-gap" (String.fromFloat pixels ++ "px")


container : HtmlElement msg
container =
    styled div
        [ displayFlex
        , flexDirection column
        , alignItems center
        , height (pct 100)
        , margin4 (px 0) (pct 25) (px 0) (pct 25)
        ]


page : HtmlElement msg
page =
    styled main_
        [ hex "#1f1f21" |> backgroundColor
        , height (pct 100)
        , width (pct 100)
        , left (px 0)
        , top (px 0)
        , hex "#e3e3e3" |> color
        , position absolute
        , fontFamilies [ "Helvetica, sans-serif" ]
        ]


tile : HtmlElement msg
tile =
    styled div
        [ border3 (px 2) solid (rgb 80 80 80)
        , width (px 50)
        , height (px 50)
        , textAlign center
        , displayFlex
        , alignItems center
        , justifyContent center
        , borderRadius (px 10)
        , fontSize (em 2)
        , textTransform uppercase
        ]


gridContainer : HtmlElement msg
gridContainer =
    styled div
        [ displayFlex
        , flexDirection column
        , width (pct 100)
        , height (pct 100)
        , alignItems center
        , gridGap 5
        ]


rowContainer : HtmlElement msg
rowContainer =
    styled div
        [ displayFlex
        , height (px 55)
        , flexDirection row
        , alignItems center
        , gridGap 5
        ]


heading : HtmlElement msg
heading =
    styled h2
        [ textTransform uppercase ]
