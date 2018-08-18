module Chess.View.Board exposing (Position, greenBorder, grid, noBorder, redBorder, square, yellowBorder)

import AppColor exposing (palette)
import Char
import Chess.Data.Board exposing (Square(..))
import Chess.View.Asset
import Html exposing (..)
import Html.Attributes exposing (..)


-- GRID


type alias Position =
    { row : Int
    , column : Char
    }


grid : { each : String, between : String } -> (Position -> a -> Html msg) -> List (List a) -> Html msg
grid areaSize toArea entries =
    let
        axisSize =
            "repeat("
                ++ toString (List.length entries)
                ++ ","
                ++ areaSize.each
                ++ ")"
    in
    entries
        |> List.indexedMap (List.indexedMap << viewArea toArea)
        |> List.concat
        |> div
            [ style
                [ ( "display", "grid" )
                , ( "justify-content", "center" )
                , ( "grid-gap", areaSize.between )
                , ( "grid-template-rows", axisSize )
                , ( "grid-template-columns", axisSize )
                ]
            ]


viewArea : (Position -> a -> Html msg) -> Int -> Int -> a -> Html msg
viewArea toArea row column entry =
    let
        position =
            { row = 8 - row
            , column = Char.fromCode (column + 97)
            }
    in
    div
        [ style
            [ ( "grid-row", toString (row + 1) )
            , ( "grid-column", toString (column + 1) )
            ]
        ]
        [ toArea position entry ]



-- SQUARES


square : Position -> Square -> List (Attribute msg) -> Html msg
square position square extraAttributes =
    div
        (style
            [ ( "width", "100%" )
            , ( "height", "100%" )
            , ( "display", "flex" )
            , ( "justify-content", "center" )
            , ( "background-color", tileBackground position )
            ]
            :: extraAttributes
        )
        [ case square of
            Empty ->
                text ""

            Occupied player piece ->
                Chess.View.Asset.svg player piece <|
                    [ style
                        [ ( "max-width", "100%" )
                        , ( "max-height", "100%" )
                        ]
                    ]
        ]


tileBackground : Position -> String
tileBackground position =
    if position.row % 2 == Char.toCode position.column % 2 then
        palette.purple
    else
        palette.gray



-- BORDERS


baseBorder : List ( String, String )
baseBorder =
    [ ( "border-width", "0.3em" )
    , ( "border-style", "solid" )
    ]


noBorder : Attribute msg
noBorder =
    style <|
        baseBorder
            ++ [ ( "border-color", "transparent" ) ]


greenBorder : Attribute msg
greenBorder =
    style <|
        baseBorder
            ++ [ ( "cursor", "pointer" )
               , ( "border-color", palette.aqua )
               ]


redBorder : Attribute msg
redBorder =
    style <|
        baseBorder
            ++ [ ( "border-color", palette.pink ) ]


yellowBorder : Attribute msg
yellowBorder =
    style <|
        baseBorder
            ++ [ ( "cursor", "pointer" )
               , ( "border-color", "yellow" )
               ]
