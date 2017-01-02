import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Debug exposing (log)

-- MODEL
type alias Model =
  { sheetId : String
  , sheetDetails : String
  }

init : String -> (Model, Cmd Msg)
init sheetId =
  ( Model sheetId "{}"
  , getSheetDetails sheetId
  )

-- UPDATE
type Msg
  = MorePlease
  | ReceiveSheetDetails (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (model, getSheetDetails model.sheetId)

    ReceiveSheetDetails (Ok newUrl) ->
      (Model model.sheetId newUrl, Cmd.none)

    ReceiveSheetDetails (Err err) ->
      case err of
        BadResponse code e ->
          log e
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
    log url
    Http.send ReceiveSheetDetails (Http.get url fetchSheetDetails)


fetchSheetDetails : Decode.Decoder String
fetchSheetDetails =
  Decode.at ["signups_count"] Decode.string

main =
  Html.program
    { init = init "sample_sheet"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

