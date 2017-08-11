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
  ( Model sheetId 0 "" "" [] [] []
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
  { id : String
  , position : Int
  , value : String
  }

type alias Column =
  { id : String
  , position : Int
  , value : String
  }

type alias SignupSlot =
  { id : String
  , rowId : String
  , columnId : String
  , maxSignups : Int
  , closed : Bool
  }

type alias Sortable a =
  { a | position : Int }

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
    , table [] [
        thead [] [viewTableColumnHeaderRow model.columns]
      , tbody [] (List.map (\row -> viewTableRow row model.signupSlots) model.rows)
      ]
    ]


sortByPosition : List (Sortable a) -> List (Sortable a)
sortByPosition sortable =
  List.sortBy .position sortable


viewTableColumnHeaderRow : List Column -> Html Msg
viewTableColumnHeaderRow columnList =
    tr [] ([(th [] [text ""] )] ++ (List.map viewColumnHeader (columnList |> sortByPosition)))

viewColumnHeader : Column -> Html Msg
viewColumnHeader column =
  th []
    [ text column.value ]

viewTableRow : Row -> List SignupSlot -> Html Msg
viewTableRow row signupSlots =
  tr [] (List.append
      [ (th [] [ text row.value ]) ]
      (viewRowSlots row signupSlots)
    )

viewRowSlots : Row -> List SignupSlot -> List (Html Msg)
viewRowSlots row signupSlotList =
  let
    rowSignupSlots = (List.filter (\slot -> slot.rowId == row.id) signupSlotList)
  in
    List.map viewSignupSlot rowSignupSlots

viewSignupSlot : SignupSlot -> Html Msg
viewSignupSlot signupSlot =
  td [] [
    text (toString signupSlot.closed)
  ]

viewSignupSlotValue : Maybe SignupSlot -> String
viewSignupSlotValue signupSlotMaybe =
    case signupSlotMaybe of
      Just signupSlot ->
        toString signupSlot.closed
      Nothing ->
        ""


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
    (field "slots" decodeSignupSlots)

decodeRows : Decoder (List Row)
decodeRows =
  Json.Decode.list decodeRow

decodeRow : Decoder Row
decodeRow =
  map3 Row
    (field "id" string)
    (field "position" int)
    (field "value" string)

decodeColumns : Decoder (List Column)
decodeColumns =
  Json.Decode.list decodeColumn

decodeColumn : Decoder Column
decodeColumn =
  map3 Column
    (field "id" string)
    (field "position" int)
    (field "value" string)

decodeSignupSlots : Decoder (List SignupSlot)
decodeSignupSlots =
  Json.Decode.list decodeSignupSlot

decodeSignupSlot : Decoder SignupSlot
decodeSignupSlot =
  map5 SignupSlot
    (field "id" string)
    (field "row_id" string)
    (field "column_id" string)
    (field "max_signups" int)
    (field "closed" bool)

main : Program Never Model Msg
main =
  Html.program
    { init = init "sheet1"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
