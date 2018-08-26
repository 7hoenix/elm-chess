module Drag exposing (Config, Msg, State, draggableAttributes, subscriptions, update)

import Animation
import Animation.Messenger as AM
import Html exposing (Attribute)
import Html.Attributes exposing (draggable)
import Html.Events exposing (onMouseEnter, onWithOptions)
import Json.Decode as D
import Mouse
import Task


type alias State a =
    { subject : Maybe a
    , position : Mouse.Position
    , original : AM.State (Msg a)
    , cursor : AM.State (Msg a)
    }


type Msg a
    = Start a Mouse.Position
    | Stop
    | Reset
    | MouseMove Mouse.Position
    | Animate Animation.Msg


type alias Config a msg =
    { toMsg : Msg a -> msg
    , finalRelease : a -> msg

    -- ANIMATIONS
    , onCursorStartDrag : List (AM.Step (Msg a))
    , onCursorStopDrag : List (AM.Step (Msg a))
    , onOriginalStartDrag : List (AM.Step (Msg a))
    , onOriginalStopDrag : List (AM.Step (Msg a))
    }


update : Config a msg -> Msg a -> State a -> ( State a, Cmd msg )
update config msg state =
    case msg of
        Start subject position ->
            ( { state
                | subject = Just subject
                , position = position
                , original = Animation.interrupt config.onOriginalStartDrag state.original
                , cursor = Animation.interrupt config.onCursorStartDrag state.cursor
              }
            , Cmd.none
            )

        Stop ->
            ( { state
                | cursor = Animation.interrupt config.onCursorStopDrag state.cursor
                , original =
                    Animation.interrupt (config.onOriginalStopDrag ++ [ AM.send Reset ])
                        state.original
              }
            , Maybe.map
                (Task.succeed >> Task.perform config.finalRelease)
                state.subject
                |> Maybe.withDefault Cmd.none
            )

        Reset ->
            ( { state | subject = Nothing }, Cmd.none )

        MouseMove position ->
            ( { state | position = position }, Cmd.none )

        Animate tick ->
            let
                ( cursor, a ) =
                    AM.update tick state.cursor

                ( original, b ) =
                    AM.update tick state.original
            in
            ( { state | original = original, cursor = cursor }
            , Cmd.map config.toMsg <| Cmd.batch [ a, b ]
            )


subscriptions : Config a msg -> State a -> Sub msg
subscriptions { toMsg } state =
    Sub.map toMsg <|
        Sub.batch
            [ Mouse.ups (\_ -> Stop)
            , Animation.subscription Animate [ state.original, state.cursor ]
            , if state.subject == Nothing then
                Sub.none

              else
                Mouse.moves MouseMove
            ]


draggableAttributes : Config a msg -> a -> List (Attribute msg)
draggableAttributes { toMsg } subject =
    [ draggable "true"
    , onDragStart <| toMsg << Start subject
    ]


onDragStart : (Mouse.Position -> msg) -> Attribute msg
onDragStart toMsg =
    onWithOptions "dragstart"
        { preventDefault = True, stopPropagation = True }
        (D.map2 (\x y -> toMsg (Mouse.Position x y))
            (D.field "clientX" D.int)
            (D.field "clientY" D.int)
        )
