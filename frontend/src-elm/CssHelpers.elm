module CssHelpers exposing (..)

import Css exposing (Color, ColorValue, Style, color, fontFamilies, pct, property, rgb, rgba, sansSerif, width)


publicSans : Style
publicSans =
    fontFamilies [ "Public Sans", .value sansSerif ]


delaGothicOne : Style
delaGothicOne =
    fontFamilies [ "Dela Gothic One", .value sansSerif ]


error : Color
error =
    rgb 255 0 0


secondary : Color
secondary =
    rgb 255 203 127


userSelectNone : Style
userSelectNone =
    property "user-select" "none"


primary : Color
primary =
    rgb 106 192 255


displayGrid : Style
displayGrid =
    property "display" "grid"


gridTemplateColumns : String -> Style
gridTemplateColumns =
    property "grid-template-columns"


gap : Int -> Style
gap i =
    property "gap" <|
        String.fromInt i
            ++ "px"


gap2 : Int -> Int -> Style
gap2 i j =
    property "gap" <|
        String.fromInt i
            ++ "px "
            ++ String.fromInt j
            ++ "px"


mx : Css.LengthOrAuto a -> Style
mx l =
    Css.batch [ Css.marginLeft l, Css.marginRight l ]


my : Css.LengthOrAuto a -> Style
my l =
    Css.batch [ Css.marginTop l, Css.marginBottom l ]


padY : Css.Length compatible units -> Style
padY l =
    Css.batch [ Css.paddingTop l, Css.paddingBottom l ]


padX : Css.Length compatible units -> Style
padX l =
    Css.batch [ Css.paddingLeft l, Css.paddingRight l ]


white : Color
white =
    rgb 255 255 255


black : Color
black =
    rgb 0 0 0


textColor : ColorValue compatible -> Style
textColor =
    color


fullWidth : Style
fullWidth =
    width (pct 100)
