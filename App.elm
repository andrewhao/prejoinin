import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)
import Debug exposing (..)

-- MODEL
type alias Model =
  { sheetId : String
  , signupCount : Int
  }

init : String -> (Model, Cmd Msg)
init sheetId =
  ( Model sheetId 0
  , getSheetDetails sheetId
  )

-- UPDATE
type Msg
  = RefreshSheet
  | ReceiveSheetDetails (Result Http.Error Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RefreshSheet ->
      (model, getSheetDetails model.sheetId)

    ReceiveSheetDetails (Ok newSignupCount) ->
      (Model model.sheetId newSignupCount, Cmd.none)

    ReceiveSheetDetails (Err err) ->
      Debug.log (toString err)
      (model, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.sheetId]
    , button [ onClick RefreshSheet ] [ text "Refresh" ]
    , br [] []
    , p [] [text (model.signupCount |> toString)]
    ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- HTTP

getSheetDetails : String -> Cmd Msg
getSheetDetails sheetId =
  let
    url =
      "//localhost:8000/mock_sheets/" ++ sheetId ++ ".json"
  in
    log url
    Http.send ReceiveSheetDetails (Http.get url fetchSheetDetails)


fetchSheetDetails : Decoder Int
fetchSheetDetails =
  at ["signups_count"] int

main =
  Html.program
    { init = init "sample_sheet"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

