module Page.SignupList exposing (Model, Msg, view, update)

import Html exposing (..)


type alias Msg =
    String


type alias Model =
    String


view : Html Msg
view =
    div [] [ text "signup list" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
