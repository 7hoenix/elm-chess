module Main exposing (Model, Msg(..), init, main, update, view)

import Chess
import Html exposing (..)



---- MODEL ----


type alias Model =
    { chess : Chess.State
    , fen : String
    }


init : ( Model, Cmd Msg )
init =
    let
        fen =
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    in
    ( { chess =
            case Chess.fromFen fen of
                Nothing ->
                    Debug.crash ""

                Just board ->
                    board
      , fen = fen
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ChessMsg Chess.Msg
    | NewFen String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChessMsg chessMsg ->
            let
                ( updatedState, cmds ) =
                    Chess.update
                        { toMsg = ChessMsg, onFenChanged = NewFen }
                        chessMsg
                        model.chess
            in
            ( { model | chess = updatedState }, cmds )

        NewFen value ->
            ( { model | fen = value }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Html.map ChessMsg <|
            Chess.view { each = "5em", between = "0.15em" } model.chess
        , Html.text model.fen
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = \model -> Sub.map ChessMsg <| Chess.subscriptions model.chess
        }
