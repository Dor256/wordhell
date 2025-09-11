module Elements exposing ( container
                         , gridContainer
                         , heading
                         , page
                         , modal
                         , rowContainer
                         , tile
                         , hitTile
                         , missTile
                         , misplacedTile
                         , keyboardContainer
                         , key
                         , keyRow
                         , spacer
                         , enterKey
                         , deleteKey
                         , transitionDelayMs
                         , HtmlElement
                         )

import Css exposing (..)
import Html.Styled exposing (Attribute, Html, div, h2, main_, styled)
import Css.Transitions as Transitions exposing (transition, ease)
import Css.Animations as Animations


type alias HtmlElement msg =
    List (Attribute msg) -> List (Html msg) -> Html msg

transitionDelayMs : Float
transitionDelayMs = 500

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

backdrop : HtmlElement msg
backdrop =
    styled div
        [ position fixed
        , top (pct 0)
        , left (pct 0)
        , width (pct 100)
        , height (pct 100)
        , backgroundColor (rgba 0 0 0 0.5)
        , zIndex (int 999)
        ]

modalContent : HtmlElement msg
modalContent =
    styled div
        [ position fixed
        , top (pct 50)
        , left (pct 50)
        , transform (translate2 (px -50) (px -50))
        , backgroundColor (hex "#3a3a3c")
        , padding (px 20)
        , borderRadius (px 10)
        , boxShadow4 (px 0) (px 0) (px 10) (hex "#000000")
        , zIndex (int 1000)
        ]

modal : Bool -> List (Html msg) -> Html msg
modal isVisible children =
    if isVisible then
        div []
            [ backdrop [] []
            , modalContent [] children
            ]
    else
        div [] []

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

flip : Animations.Keyframes {}
flip =
    Animations.keyframes
        [ (0, [ Animations.transform [rotateX (deg 0)] ])
        , (50, [ Animations.transform [rotateX (deg 90)] ])
        ]

flipAnimation : Int -> List Style
flipAnimation index =
    [ animationName flip
    , animationDuration (ms 700)
    , animationDelay (ms (toFloat index * transitionDelayMs))
    ]

backgroundColorTransition : Int -> Style
backgroundColorTransition index =
    transition [ Transitions.backgroundColor3 2000 (toFloat index * transitionDelayMs) ease ]

hitTile : Int -> HtmlElement msg
hitTile index =
    styled tile <|
        [ backgroundColor (hex "#538d4e")
        , backgroundColorTransition index
        ] ++ flipAnimation index

missTile : Int -> HtmlElement msg
missTile index =
    styled tile <|
        [ backgroundColor (hex "#3a3a3c")
        , backgroundColorTransition index
        ] ++ flipAnimation index

misplacedTile : Int -> HtmlElement msg
misplacedTile index =
    styled tile <|
        [ backgroundColor (hex "#b59f3b")
        , backgroundColorTransition index
        ] ++ flipAnimation index

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


keyboardContainer : HtmlElement msg
keyboardContainer =
    styled container
        [ marginTop (pct 5)
        , width (pct 25)
        , flexDirection column
        ]

key : HtmlElement msg
key =
    styled div
        [ fontWeight bold
        , margin4 (px 0) (px 6) (px 0) (px 0)
        , height (px 58)
        , borderRadius (px 4)
        , cursor pointer
        , property "user-select" "none"
        , flex (int 1)
        , displayFlex
        , justifyContent center
        , alignItems center
        , textTransform uppercase
        ]

keyRow : HtmlElement msg
keyRow =
    styled div
        [ displayFlex
        , width (pct 100)
        , margin3 (px 0) auto (px 8)
        ]

enterKey : HtmlElement msg
enterKey =
    styled key
        [ backgroundColor (hex "#818384")
        , flex (num 1.5)
        , padding2 (px 0) (px 5)
        ]

deleteKey : HtmlElement msg
deleteKey =
    styled key
        [ backgroundColor (hex "#818384")
        , flex (num 1.5)
        , fontSize (px 25)
        , padding2 (px 0) (px 5)
        ]

spacer : HtmlElement msg
spacer =
    styled div
        [ flex (num 0.5) ]

heading : HtmlElement msg
heading =
    styled h2
        [ textTransform uppercase
        , borderBottom3 (px 1) solid (hex "#3a3a3c")
        , width (pct 100)
        , textAlign center
        , paddingBottom (px 16)
        ]
