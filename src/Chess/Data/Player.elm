module Chess.Data.Player exposing (Player(..), cpu, player, user)

import Json.Decode exposing (..)


type Player
    = White
    | Black


user : Player
user =
    White


cpu : Player
cpu =
    Black


player : Decoder Player
player =
    andThen parsePlayer string


parsePlayer : String -> Decoder Player
parsePlayer raw =
    case raw of
        "CPU" ->
            succeed cpu

        "USER" ->
            succeed user

        invalid ->
            fail <| "`" ++ invalid ++ "` is not a valid player"
