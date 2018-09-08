module Chess.View.Board exposing
    ( Config
    , greenBorder
    , grid
    , noBorder
    , redBorder
    , square
    , yellowBorder
    )

{-|

@docs Config
@docs greenBorder
@docs grid
@docs noBorder
@docs redBorder
@docs square
@docs yellowBorder

-}

import Chess.Data.Board exposing (Square(..))
import Chess.Data.Position exposing (Position)
import Chess.View.Asset
import Html exposing (..)
import Html.Attributes exposing (..)
import Palette


{-| -}
type alias Config =
    { each : String
    , between : String
    , borderSize : String
    }



-- GRID


{-| -}
grid : Config -> (Position -> a -> Html msg) -> List (List a) -> Html msg
grid viewConfig toArea entries =
    let
        axisSize =
            "repeat("
                ++ toString (List.length entries)
                ++ ","
                ++ viewConfig.each
                ++ ")"
    in
    entries
        |> List.indexedMap (List.indexedMap << viewArea toArea)
        |> List.concat
        |> div
            [ style
                [ ( "display", "grid" )
                , ( "justify-content", "center" )
                , ( "grid-gap", viewConfig.between )
                , ( "grid-template-rows", axisSize )
                , ( "grid-template-columns", axisSize )
                ]
            ]


viewArea : (Position -> a -> Html msg) -> Int -> Int -> a -> Html msg
viewArea toArea row column entry =
    let
        position =
            Chess.Data.Position.fromRowColumn row column
    in
    div
        [ style
            [ ( "grid-row", toString (row + 1) )
            , ( "grid-column", toString (column + 1) )
            ]
        ]
        [ toArea position entry ]



-- SQUARES


{-| -}
square : Position -> Square -> List (Attribute msg) -> List (Attribute msg) -> Html msg
square position square squareAttributes svgAttributes =
    div
        (style
            [ ( "width", "100%" )
            , ( "height", "100%" )
            , ( "display", "flex" )
            , ( "justify-content", "center" )
            , ( "background-color", tileBackground position )
            ]
            :: squareAttributes
        )
        [ case square of
            Empty ->
                text ""

            Occupied player piece ->
                Chess.View.Asset.findSvg player piece <|
                    (style
                        [ ( "max-height", "100%" )
                        , ( "max-width", "100%" )
                        , ( "pointer-events", "none" )
                        ]
                        :: svgAttributes
                    )
        ]


tileBackground : Position -> String
tileBackground position =
    if Chess.Data.Position.alongDiagonal position then
        Palette.purple

    else
        Palette.gray



-- BORDERS


baseBorder : Config -> List ( String, String )
baseBorder config =
    [ ( "border-width", config.borderSize )
    , ( "border-style", "solid" )
    ]


{-| -}
noBorder : Config -> Attribute msg
noBorder config =
    style <|
        baseBorder config
            ++ [ ( "border-color", "transparent" ) ]


{-| -}
greenBorder : Config -> Attribute msg
greenBorder config =
    style <|
        baseBorder config
            ++ [ ( "cursor", "pointer" )
               , ( "border-color", Palette.aqua )
               ]


{-| -}
redBorder : Config -> Attribute msg
redBorder config =
    style <|
        baseBorder config
            ++ [ ( "border-color", Palette.pink ) ]


{-| -}
yellowBorder : Config -> Attribute msg
yellowBorder config =
    style <|
        baseBorder config
            ++ [ ( "cursor", "pointer" )
               , ( "border-color", Palette.yellow )
               ]
