module Chess exposing
    ( Msg
    , State
    , fromFen
    , subscriptions
    , update
    , view
    )

{-|

@docs Msg
@docs State
@docs fromFen
@docs subscriptions
@docs update
@docs view

-}

import Animation
import Chess.Data.Board exposing (Board, Square(..))
import Chess.Data.Piece exposing (Piece(..))
import Chess.Data.Player exposing (Player(..))
import Chess.View.Asset
import Chess.View.Board
import Drag
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Mouse
import Task



---- MODEL ----


{-| -}
type State
    = State
        { board : Board
        , drag : Drag.State DraggableItem
        , hover : Maybe Chess.View.Board.Position
        }


type alias DraggableItem =
    { position : Chess.View.Board.Position
    , player : Player
    , piece : Piece
    }


type alias Move =
    { from : Chess.View.Board.Position
    , to : Chess.View.Board.Position
    , player : Player
    , piece : Piece
    }


{-| -}
fromFen : String -> Maybe State
fromFen fen =
    case D.decodeValue Chess.Data.Board.board (E.string fen) of
        Err reason ->
            Nothing

        Ok board ->
            Just <|
                State
                    { board = board
                    , hover = Nothing
                    , drag =
                        { subject = Nothing
                        , position = Mouse.Position 0 0
                        , original = Animation.style present
                        , cursor = Animation.style gone
                        }
                    }



--- UPDATE ----


{-| -}
type Msg
    = SetHover Chess.View.Board.Position
    | DragMsg (Drag.Msg DraggableItem)
    | FinalRelease DraggableItem


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg (State state) =
    case msg of
        SetHover position ->
            ( State { state | hover = Just position }, Cmd.none )

        DragMsg dragMsg ->
            Drag.update dragConfig dragMsg state.drag
                |> Tuple.mapFirst (\drag -> State { state | drag = drag })

        FinalRelease from ->
            case state.hover of
                Nothing ->
                    ( State state, Cmd.none )

                Just to ->
                    ( makeMove to (State state), Cmd.none )


dragConfig : Drag.Config DraggableItem Msg
dragConfig =
    { toMsg = DragMsg
    , finalRelease = FinalRelease

    -- ANIMATION
    , onCursorStartDrag = [ Animation.to present ]
    , onCursorStopDrag = [ Animation.to gone ]
    , onOriginalStartDrag = [ Animation.to gone ]
    , onOriginalStopDrag = [ Animation.to present ]
    }


makeMove : Chess.View.Board.Position -> State -> State
makeMove to (State state) =
    case state.drag.subject of
        Nothing ->
            State state

        Just sub ->
            let
                move =
                    Move
                        sub.position
                        to
                        sub.player
                        sub.piece

                updatedBoard =
                    makeMoveOnBoard move state.board
            in
            State { state | board = updatedBoard }


makeMoveOnBoard : Move -> Board -> Board
makeMoveOnBoard move board =
    board
        |> removePieceFromBoard move
        |> addPieceToBoard move


removePieceFromBoard : Move -> Board -> Board
removePieceFromBoard { from } board =
    List.indexedMap
        (\x row ->
            List.indexedMap
                (\y square ->
                    if toRow x == from.row && toColumn y == from.column then
                        Empty

                    else
                        square
                )
                row
        )
        board


addPieceToBoard : Move -> Board -> Board
addPieceToBoard { to, player, piece } board =
    List.indexedMap
        (\x row ->
            List.indexedMap
                (\y square ->
                    if toRow x == to.row && toColumn y == to.column then
                        Occupied player piece

                    else
                        square
                )
                row
        )
        board


toRow : Int -> Int
toRow row =
    row + 1


toColumn : Int -> Char
toColumn column =
    case column + 1 of
        1 ->
            'a'

        2 ->
            'b'

        3 ->
            'c'

        4 ->
            'd'

        5 ->
            'e'

        6 ->
            'f'

        7 ->
            'g'

        8 ->
            'h'

        _ ->
            Debug.crash "Column parsing failure"


present : List Animation.Property
present =
    [ Animation.opacity 1.0 ]


gone : List Animation.Property
gone =
    [ Animation.opacity 0.0 ]


{-| -}
subscriptions : State -> Sub Msg
subscriptions (State { board, drag }) =
    Drag.subscriptions dragConfig drag



---- VIEW ----


{-| -}
type alias ViewConfig =
    { each : String, between : String }


{-| -}
view : ViewConfig -> State -> Html Msg
view config (State { board, drag }) =
    Html.span []
        [ Chess.View.Board.grid config (viewCell drag) board
        , viewGhostImage config drag
        ]


viewGhostImage : ViewConfig -> Drag.State DraggableItem -> Html Msg
viewGhostImage config drag =
    case drag.subject of
        Nothing ->
            Html.text ""

        Just { player, piece } ->
            Chess.View.Asset.svg player
                piece
                (Animation.render drag.cursor
                    ++ [ followCursor drag.position
                       , Html.Attributes.style
                            [ ( "max-width", config.each )
                            , ( "max-height", config.each )
                            ]
                       ]
                )


followCursor : Mouse.Position -> Attribute msg
followCursor { x, y } =
    Html.Attributes.style
        [ ( "position", "absolute" )
        , ( "top", "calc(" ++ toString y ++ "px - 1em)" )
        , ( "left", "calc(" ++ toString x ++ "px - 1.10em)" )
        , ( "z-index", "9" )
        ]


viewCell : Drag.State DraggableItem -> Chess.View.Board.Position -> Chess.Data.Board.Square -> Html Msg
viewCell drag position square =
    case square of
        Empty ->
            Chess.View.Board.square position
                square
                [ onMouseEnter (SetHover position) ]
                []

        Occupied player piece ->
            Chess.View.Board.square position
                square
                (onMouseEnter (SetHover position)
                    :: Html.Attributes.style [ ( "cursor", "grab" ) ]
                    :: Drag.draggableAttributes dragConfig
                        (DraggableItem position player piece)
                )
                (animateDrag position drag)


animateDrag : Chess.View.Board.Position -> Drag.State DraggableItem -> List (Attribute Msg)
animateDrag position drag =
    if Maybe.map .position drag.subject == Just position then
        Animation.render drag.original

    else
        []
