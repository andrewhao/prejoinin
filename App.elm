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
  , title : String
  , description : String
  , rows: List Row
  , columns: List Column
  , signupSlots: List SignupSlot
  }

init : String -> (Model, Cmd Msg)
init sheetId =
  ( Model sheetId
  , getSheetDetails sheetId
  )

-- UPDATE
type Msg
  = FetchSheet
  | ReceiveSheetDetails (Result Http.Error SheetJSONResponse)
  | ChangeSheetID String

type alias LegacySheetJSONResponse =
  { signupCount : Int
  , title : String
  , description : String
  }

type alias SheetJSONResponse =
  { title : String
  , description : String
  , rows : List Row
  , columns : List Column
  , signupSlots : List SignupSlot
  }

type alias Row =
  { id : Int
  , position : Int
  , value : String
  }

type alias Column =
  { id : Int
  , position : Int
  , value : String
  }

type alias SignupSlot =
  { id : Int
  , rowId : Int
  , columnId : Int
  , maxSignups : Int
  , closed : Bool
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchSheet ->
      (model, getSheetDetails model.sheetId)

    ReceiveSheetDetails (Ok jsonResponse) ->
      (Model model.sheetId
             0
             jsonResponse.title
             jsonResponse.description
             jsonResponse.rows
             jsonResponse.columns
             jsonResponse.signupSlots
      , Cmd.none)

    ReceiveSheetDetails (Err err) ->
      Debug.log (toString err)
      (model, Cmd.none)

    ChangeSheetID newSheetId ->
      ({ model | sheetId = newSheetId }
      , Cmd.none)

-- VIEW
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


fetchSheetDetails : Decoder SheetJSONResponse
fetchSheetDetails =
  map5 SheetJSONResponse
    (field "title" string)
    (field "description" string)
    (field "rows" decodeRows)
    (field "columns" decodeColumns)
    (field "signup_slots" decodeSignupSlots)

decodeRows : Decoder (List Row)
decodeRows =
  map3 Row
    (field "id" int)
    (field "position" int)
    (field "value" string)

decodeColumns : Decoder (List Column)
decodeColumns =
  map3 Column
    (field "id" int)
    (field "position" int)
    (field "value" string)


decodeSignupSlots : Decoder (List SignupSlot)
decodeSignupSlots =
  map5 SignupSlot
    (field "id" int)
    (field "row_id" int)
    (field "column_id" int)
    (field "max_signups" int)
    (field "closed" bool)

main =
  Html.program
    { init = init "sheet1"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

