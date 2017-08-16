module SignupTable
    exposing
        ( Msg(..)
        , Model
        , init
        , update
        , view
        , subscriptions
        )

import Data.Sheet exposing (Column, Row, SheetJSONResponse, Signup, SignupSlot, decodeColumns, decodeRows, decodeSignupSlots, decodeSignups)
import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, placeholder)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (..)


-- MESSAGES


type Msg
    = FetchSheet
    | ReceiveSheetDetails (Result Http.Error SheetJSONResponse)
    | ChangeSheetID String
    | FocusSlotJoin String



-- INIT


init : String -> ( Model, Cmd Msg )
init sheetId =
    ( Model sheetId "" "" [] [] [] []
    , getSheetDetails sheetId
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchSheet ->
            ( model, getSheetDetails model.sheetId )

        ReceiveSheetDetails (Ok jsonResponse) ->
            ( Model model.sheetId
                jsonResponse.title
                jsonResponse.description
                jsonResponse.rows
                jsonResponse.columns
                jsonResponse.signupSlots
                jsonResponse.signups
            , Cmd.none
            )

        ReceiveSheetDetails (Err err) ->
            Debug.log (toString err)
                ( model, Cmd.none )

        ChangeSheetID newSheetId ->
            ( { model | sheetId = newSheetId }
            , Cmd.none
            )

        FocusSlotJoin slotID ->
            ( model, Cmd.none )


type alias Sortable a =
    { a | position : Int }


type alias Model =
    { sheetId : String
    , title : String
    , description : String
    , rows : List Row
    , columns : List Column
    , signupSlots : List SignupSlot
    , signups : List Signup
    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Sheet ID", onInput ChangeSheetID ] []
        , button [ onClick FetchSheet ] [ text "Fetch" ]
        , h1 [] [ text (model.title) ]
        , p [] [ text (model.description) ]
        , viewTable model
        ]


viewTable : Model -> Html Msg
viewTable model =
    div []
        [ table []
            [ thead [] [ viewTableColumnHeaderRow model.columns ]
            , tbody [] (List.map (\row -> viewTableRow row model.signupSlots model.signups) model.rows)
            ]
        ]


sortByPosition : List (Sortable a) -> List (Sortable a)
sortByPosition sortable =
    List.sortBy .position sortable


viewTableColumnHeaderRow : List Column -> Html Msg
viewTableColumnHeaderRow columnList =
    tr [] ([ (th [] [ text "" ]) ] ++ (List.map viewColumnHeader (columnList |> sortByPosition)))


viewColumnHeader : Column -> Html Msg
viewColumnHeader column =
    th []
        [ text column.value ]


viewTableRow : Row -> List SignupSlot -> List Signup -> Html Msg
viewTableRow row signupSlotList signupList =
    tr []
        (List.append
            [ (th [] [ text row.value ]) ]
            (viewRowSlots row signupSlotList signupList)
        )


viewRowSlots : Row -> List SignupSlot -> List Signup -> List (Html Msg)
viewRowSlots row signupSlotList signupList =
    let
        rowSignupSlots =
            (List.filter (\slot -> slot.rowId == row.id) signupSlotList)
    in
        List.map (\rowSignupSlot -> viewSignupSlot rowSignupSlot signupList) rowSignupSlots


viewSignupSlot : SignupSlot -> List Signup -> Html Msg
viewSignupSlot signupSlot signupList =
    td []
        [ div [ class "signups" ] (viewSignupsForSlot signupSlot signupList)
        , (if signupSlot.closed then
            button [ class "join", disabled True ] [ text "Join ->" ]
           else
            button [ class "join", onClick (FocusSlotJoin signupSlot.id) ] [ text "Join ->" ]
          )
        ]


viewSignupSlotValue : Maybe SignupSlot -> String
viewSignupSlotValue signupSlotMaybe =
    case signupSlotMaybe of
        Just signupSlot ->
            toString signupSlot.closed

        Nothing ->
            ""


viewSignupsForSlot : SignupSlot -> List Signup -> List (Html Msg)
viewSignupsForSlot signupSlot signupList =
    let
        filteredSignups =
            signupsForSlot signupSlot signupList
    in
        List.map viewSignup filteredSignups


viewSignup : Signup -> Html Msg
viewSignup signup =
    div [] [ text (signup.name ++ "(" ++ signup.comment ++ ")") ]


signupsForSlot : SignupSlot -> List Signup -> List Signup
signupsForSlot signupSlot signupList =
    List.filter (\signup -> signup.signupSlotId == signupSlot.id) signupList



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getSheetDetails : String -> Cmd Msg
getSheetDetails sheetId =
    let
        url =
            "//localhost:3000/api/v1/sheets/" ++ sheetId ++ ".json"
    in
        log url
            Http.send
            ReceiveSheetDetails
            (Http.get url fetchSheetDetails)


fetchSheetDetails : Decoder SheetJSONResponse
fetchSheetDetails =
    map6 SheetJSONResponse
        (field "title" string)
        (field "description" string)
        (field "rows" decodeRows)
        (field "columns" decodeColumns)
        (field "signup_slots" decodeSignupSlots)
        (field "signups" decodeSignups)
