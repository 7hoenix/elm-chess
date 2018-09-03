module Chess.View.Asset exposing (svg)

{-|

@docs svg

-}

import Chess.Data.Piece exposing (Piece(..))
import Chess.Data.Player exposing (Player(..))
import Html exposing (Attribute, Html, object)
import Html.Attributes exposing (attribute, style, type_)


{-| -}
svg : Player -> Piece -> List (Attribute msg) -> Html msg
svg player piece extraAttrs =
    let
        allAttrs =
            type_ "image/svg+xml"
                :: style [ ( "pointer-events", "none" ) ]
                :: data player piece
                :: extraAttrs
    in
    object allAttrs []


data : Player -> Piece -> Attribute msg
data player piece =
    case player of
        Black ->
            case piece of
                Pawn ->
                    attribute "data" "/images/black-pawn.svg"

                Rook ->
                    attribute "data" "/images/black-rook.svg"

                Knight ->
                    attribute "data" "/images/black-knight.svg"

                Bishop ->
                    attribute "data" "/images/black-bishop.svg"

                Queen ->
                    attribute "data" "/images/black-queen.svg"

                King ->
                    attribute "data" "/images/black-king.svg"

        White ->
            case piece of
                Pawn ->
                    attribute "data" "/images/white-pawn.svg"

                Rook ->
                    attribute "data" "/images/white-rook.svg"

                Knight ->
                    attribute "data" "/images/white-knight.svg"

                Bishop ->
                    attribute "data" "/images/white-bishop.svg"

                Queen ->
                    attribute "data" "/images/white-queen.svg"

                King ->
                    attribute "data" "/images/white-king.svg"
