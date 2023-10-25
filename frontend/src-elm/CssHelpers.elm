module CssHelpers exposing (..)

import Css exposing (Style, fontFamilies, sansSerif)


publicSans : Style
publicSans =
    fontFamilies [ "Public Sans", .value sansSerif ]
