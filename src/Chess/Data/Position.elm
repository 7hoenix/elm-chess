module Chess.Data.Position exposing (Position, alongDiagonal, fromRowColumn, toRowColumn)

import Char


type Position
    = Position Int Char


alongDiagonal : Position -> Bool
alongDiagonal (Position row column) =
    row % 2 == Char.toCode column % 2


fromRowColumn : Int -> Int -> Position
fromRowColumn row column =
    Position
        (row + 1)
        (Char.fromCode (column + 97))


toRowColumn : Position -> ( Int, Int )
toRowColumn (Position row column) =
    ( row - 1, Char.toCode column - 97 )
