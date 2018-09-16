module Chess.Data.Board exposing
    ( Board
    , Square(..)
    , board
    , toFen
    )

{-|

@docs Board
@docs Square
@docs board
@docs toFen

-}

import Char
import Chess.Data.Piece exposing (Piece(..))
import Chess.Data.Player as Player
import Dict exposing (Dict)
import Json.Decode exposing (..)


{-| -}
type alias Board =
    List (List Square)


{-| -}
type Square
    = Empty
    | Occupied Player.Player Piece


{-| -}
board : Decoder Board
board =
    andThen parseFen string


parseFen : String -> Decoder Board
parseFen entireGameState =
    String.words entireGameState
        |> List.head
        |> parseFenHelp


parseFenHelp : Maybe String -> Decoder Board
parseFenHelp entireBoard =
    case entireBoard of
        Just rows ->
            String.split "/" rows
                |> List.foldr (parseFenRow >> map2 (::)) (succeed [])

        Nothing ->
            fail "not a FEN-encoded board"


parseFenRow : String -> Decoder (List Square)
parseFenRow row =
    String.toList row
        |> List.foldl parseFenRowHelp (succeed [])


parseFenRowHelp : Char -> Decoder (List Square) -> Decoder (List Square)
parseFenRowHelp nextColumn accumulator =
    map2 (++) accumulator (parseFenColumn nextColumn)


parseFenColumn : Char -> Decoder (List Square)
parseFenColumn c =
    if Char.isDigit c then
        succeed <| List.repeat (parseDigit c) Empty

    else
        map List.singleton (piece c)


piece : Char -> Decoder Square
piece c =
    case Dict.get c allPieces of
        Just c ->
            succeed c

        Nothing ->
            fail "not a FEN-encoded piece"


allPieces : Dict Char Square
allPieces =
    Dict.fromList
        [ ( 'P', Occupied Player.White Pawn )
        , ( 'R', Occupied Player.White Rook )
        , ( 'N', Occupied Player.White Knight )
        , ( 'B', Occupied Player.White Bishop )
        , ( 'Q', Occupied Player.White Hand )
        , ( 'K', Occupied Player.White Monarch )
        , ( 'p', Occupied Player.Black Pawn )
        , ( 'r', Occupied Player.Black Rook )
        , ( 'n', Occupied Player.Black Knight )
        , ( 'b', Occupied Player.Black Bishop )
        , ( 'q', Occupied Player.Black Hand )
        , ( 'k', Occupied Player.Black Monarch )
        ]


parseDigit : Char -> Int
parseDigit c =
    Char.toCode c - 48



-- TO FEN


{-| -}
toFen : Board -> String
toFen board =
    List.map toFenRow board
        |> String.join "/"
        |> withFenSuffix


{-| TODO: This does not capture en-passent,
castling, turn, or other niceties of FEN.
-}
withFenSuffix : String -> String
withFenSuffix rows =
    rows ++ " w - - 0 1"


toFenRow : List Square -> String
toFenRow =
    List.foldl toFenRowHelp (Accumulator "" 0) >> collapseEmpties


type alias Accumulator =
    { output : String, empties : Int }


toFenRowHelp : Square -> Accumulator -> Accumulator
toFenRowHelp square accumulator =
    case square of
        Empty ->
            { accumulator | empties = accumulator.empties + 1 }

        Occupied player piece ->
            { empties = 0
            , output =
                collapseEmpties accumulator
                    ++ occupiedString player piece
            }


collapseEmpties : Accumulator -> String
collapseEmpties { output, empties } =
    if empties == 0 then
        output

    else
        output ++ toString empties


occupiedString : Player.Player -> Piece -> String
occupiedString player piece =
    case ( player, piece ) of
        ( Player.White, Pawn ) ->
            "P"

        ( Player.White, Rook ) ->
            "R"

        ( Player.White, Knight ) ->
            "N"

        ( Player.White, Bishop ) ->
            "B"

        ( Player.White, Hand ) ->
            "Q"

        ( Player.White, Monarch ) ->
            "K"

        ( Player.Black, Pawn ) ->
            "p"

        ( Player.Black, Rook ) ->
            "r"

        ( Player.Black, Knight ) ->
            "n"

        ( Player.Black, Bishop ) ->
            "b"

        ( Player.Black, Hand ) ->
            "q"

        ( Player.Black, Monarch ) ->
            "k"
