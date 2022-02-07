module Elements exposing (container, page, letter)

import Css exposing (..)
import Html.Styled exposing (Html, Attribute, styled, div, main_)

type alias HtmlElement msg = List (Attribute msg) -> List (Html msg) -> Html msg

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
      , hex "#e3e3e3" |> color
      , position absolute
      ]

letter : HtmlElement msg
letter =
  styled div
    [ border3 (px 2) solid (rgb 194 0 0)
    , width (pct 5)
    , height (pct 10)
    , marginLeft (pct 2)
    , marginRight (pct 2)
    ]
