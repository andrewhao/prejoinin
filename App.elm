import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task

-- MODEL
type alias Model =
  { sheetId : String
  , sheetDetails : String
  }

init : String -> (Model, Cmd Msg)
init sheetId =
  ( Model sheetId "sample_sheet", sheetDetails "{}"
  , getSheetDetails sheetId
  )

-- UPDATE
type Msg
  = MorePlease
  | FetchSucceed String
  | FetchFail Http.Error


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (model, getSheetDetails model.sheetId)

    FetchSucceed newUrl ->
      (Model model.sheetId newUrl, Cmd.none)

    FetchFail _ ->
      (model, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.sheetId]
    , button [ onClick MorePlease ] [ text "More Please!" ]
    , br [] []
    , p [] [text model.sheetDetails]
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
    Task.perform FetchFail FetchSucceed (Http.get fetchSheetDetails url)

fetchSheetDetails : Json.Decoder String
fetchSheetDetails =
  Json.at ["signups_count"] Json.string


main =
  Html.program
    { init = init "sample_sheet"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

