module Chess.Data.Board exposing (Board, Square(..), board)

import Char
import Chess.Data.Piece exposing (Piece(..))
import Chess.Data.Player as Player
import Dict exposing (Dict)
import Json.Decode exposing (..)


type alias Board =
    List (List Square)


type Square
    = Empty
    | Occupied Player.Player Piece


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
        , ( 'Q', Occupied Player.White Queen )
        , ( 'K', Occupied Player.White King )
        , ( 'p', Occupied Player.Black Pawn )
        , ( 'r', Occupied Player.Black Rook )
        , ( 'n', Occupied Player.Black Knight )
        , ( 'b', Occupied Player.Black Bishop )
        , ( 'q', Occupied Player.Black Queen )
        , ( 'k', Occupied Player.Black King )
        ]


parseDigit : Char -> Int
parseDigit c =
    Char.toCode c - 48
