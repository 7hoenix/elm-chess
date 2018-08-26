module Main exposing (..)

import Chess
import Html exposing (..)


---- MODEL ----


type alias Model =
    { chess : Chess.State
    }


init : ( Model, Cmd Msg )
init =
    ( { chess =
            case Chess.fromFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" of
                Nothing ->
                    Debug.crash ""

                Just board ->
                    board
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ChessMsg Chess.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update (ChessMsg msg) model =
    let
        ( updatedState, cmds ) =
            Chess.update msg model.chess
    in
    ( { model | chess = updatedState }, Cmd.map ChessMsg cmds )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Html.map ChessMsg <|
            Chess.view { each = "5em", between = "0.15em" } model.chess
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
