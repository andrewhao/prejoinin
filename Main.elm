module Main exposing (..)

import Html exposing (..)
import SignupTable exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init "2"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
