import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode

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
  | NewGif (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (model, getSheetDetails model.sheetId)

    NewGif (Ok newUrl) ->
      (Model model.sheetId newUrl, Cmd.none)

    NewGif (Err _) ->
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
    Http.send NewGif (Http.get url fetchSheetDetails)


fetchSheetDetails : Decode.Decoder String
fetchSheetDetails =
  Decode.at ["signups_count"] Decode.string

main =
  Html.program
    { init = init "cats"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

