import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import SignupTable exposing (..)

--- VIEW
view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Sheet ID", onInput ChangeSheetID ] []
    , h2 [] [text model.sheetId]
    , button [ onClick FetchSheet ] [ text "Fetch" ]
    , br [] []
    , p [] [text (model.signupCount |> toString)]
    , h1 [] [text (model.title)]
    , p [] [text (model.description)]
    , SignupTable.view model
    ]


main : Program Never Model Msg
main =
  Html.program
    { init = init "sheet1"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
