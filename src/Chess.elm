module Chess exposing (State, fromFen, view)

import AppColor
import Chess.Data.Board exposing (Board)
import Chess.View.Board
import Html exposing (Html)
import Json.Decode as D
import Json.Encode as E


---- MODEL ----


type State
    = State Board


fromFen : String -> Maybe State
fromFen fen =
    case D.decodeValue Chess.Data.Board.board (E.string fen) of
        Err reason ->
            Nothing

        Ok board ->
            Just <| State board



---- VIEW ----


type alias ViewConfig =
    { each : String, between : String }


view : ViewConfig -> State -> Html msg
view config (State board) =
    Chess.View.Board.grid config viewCell board


viewCell : Chess.View.Board.Position -> Chess.Data.Board.Square -> Html msg
viewCell position square =
    Chess.View.Board.square position square []
