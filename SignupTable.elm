module SignupTable
    exposing
        ( Msg(..)
        , Model
        , init
        , update
        , view
        , subscriptions
        )

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Data.Sheet exposing (Column, Row, SheetJSONResponse, Signup, SignupJSONResponse, SignupSlot, decodeColumns, decodeRows, decodeSignupSlots, decodeSignups)
import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, classList, disabled, for, href, name, placeholder, required, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit, onWithOptions)
import Http
import Json.Decode exposing (..)
import Json.Encode


-- Bootstrap style helpers

import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Table as Table


-- MESSAGES


type Msg
    = FetchSheet
    | ReceiveSheetDetails (Result Http.Error SheetJSONResponse)
    | ReceiveSignupResponse (Result Http.Error SignupJSONResponse)
    | ChangeSheetID SheetID
    | FocusSlotJoin SignupSlotID
    | EditNewSignupName String
    | EditNewSignupEmail String
    | EditNewSignupComment String
    | CancelSlotFocus SignupSlotID
    | SubmitNewSignup


type alias SheetID =
    String


type alias RowID =
    String


type alias SignupSlotID =
    String


type alias SignupID =
    String


type alias Model =
    { sheetId : SheetID
    , title : String
    , description : String
    , rows : List Row
    , columns : List Column
    , signupSlots : List SignupSlot
    , signups : List Signup
    , focusedSlotId : Maybe SignupSlotID
    , currentNewSignupName : Maybe String
    , currentNewSignupEmail : Maybe String
    , currentNewSignupComment : Maybe String
    }


type alias Sortable a =
    { a | position : Int }



-- A Sheet minimally composes these attributes
--


type alias Sheet a =
    { a
        | sheetId : SheetID
        , title : String
        , description : String
        , rows : List Row
        , columns : List Column
        , signupSlots : List SignupSlot
        , signups : List Signup
    }



-- INIT


init : String -> ( Model, Cmd Msg )
init sheetId =
    ( Model sheetId
        ""
        ""
        []
        []
        []
        []
        Nothing
        Nothing
        Nothing
        Nothing
    , getSheetDetails sheetId
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchSheet ->
            ( model, getSheetDetails model.sheetId )

        SubmitNewSignup ->
            ( model, createSignup model )

        ReceiveSignupResponse (Err err) ->
            log (toString err)
                ( model, Cmd.none )

        ReceiveSignupResponse (Ok jsonResponse) ->
            ( model |> defocusSlot
            , getSheetDetails model.sheetId
            )

        ReceiveSheetDetails (Ok jsonResponse) ->
            ( Model
                model.sheetId
                jsonResponse.title
                jsonResponse.description
                jsonResponse.rows
                jsonResponse.columns
                jsonResponse.signupSlots
                jsonResponse.signups
                model.focusedSlotId
                model.currentNewSignupName
                model.currentNewSignupEmail
                model.currentNewSignupComment
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
            ( { model | focusedSlotId = Just slotID }, Cmd.none )

        EditNewSignupName newName ->
            ( { model | currentNewSignupName = Just newName }, Cmd.none )

        EditNewSignupEmail newEmail ->
            ( { model | currentNewSignupEmail = Just newEmail }, Cmd.none )

        EditNewSignupComment newComment ->
            ( { model | currentNewSignupComment = Just newComment }, Cmd.none )

        CancelSlotFocus slotID ->
            ( defocusSlot model
            , Cmd.none
            )


defocusSlot : Model -> Model
defocusSlot model =
    { model
        | focusedSlotId = Nothing
        , currentNewSignupName = Nothing
        , currentNewSignupEmail = Nothing
        , currentNewSignupComment = Nothing
    }



--
-- VIEW


view : Model -> Html Msg
view model =
    Grid.containerFluid []
        [ Grid.row []
            [ Grid.col []
                [ viewDevelopmentDebugHeader model
                , h1 [] [ text (model.title) ]
                , p [] [ text (model.description) ]
                , viewTable model
                ]
            ]
        ]


viewDevelopmentDebugHeader : Model -> Html Msg
viewDevelopmentDebugHeader model =
    Card.config []
        |> Card.header []
            [ text "DEVELOPMENT MODE" ]
        |> Card.block []
            [ Card.text []
                [ Form.form []
                    [ Form.group []
                        [ Form.label [ for "dev_sheet_id" ] [ text "Sheet ID" ]
                        , Input.text [ Input.id "dev_sheet_id", Input.onInput ChangeSheetID, Input.value model.sheetId ]
                        , Form.help [] [ text "Sheet ID you wish to query for" ]
                        , Button.button [ Button.primary, Button.onClick FetchSheet ] [ text "Fetch" ]
                        ]
                    ]
                ]
            ]
        |> Card.view


viewTable : Model -> Html Msg
viewTable model =
    div []
        [ Table.table
            { options = []
            , thead = Table.thead [] [ viewTableColumnHeaderRow model.columns ]
            , tbody = Table.tbody [] (List.map (\row -> viewTableRow row model) model.rows)
            }
        ]


sortByPosition : List (Sortable a) -> List (Sortable a)
sortByPosition sortable =
    List.sortBy .position sortable


viewTableColumnHeaderRow : List Column -> Table.Row Msg
viewTableColumnHeaderRow columnList =
    Table.tr [] ([ (Table.th [] [ text "" ]) ] ++ (List.map viewColumnHeader (columnList |> sortByPosition)))


viewColumnHeader : Column -> Table.Cell Msg
viewColumnHeader column =
    Table.th []
        [ text column.value ]


viewTableRow : Row -> Model -> Table.Row Msg
viewTableRow row model =
    let
        signupSlotList =
            model.signupSlots

        signupList =
            model.signups
    in
        Table.tr []
            (List.append
                [ (Table.th [] [ text row.value ]) ]
                (viewRowSlots row model)
            )


viewRowSlots : Row -> Model -> List (Table.Cell Msg)
viewRowSlots row model =
    let
        signupList =
            model.signups

        rowSignupSlots =
            (List.filter (\slot -> slot.rowId == row.id) model.signupSlots)
    in
        List.map (\rowSignupSlot -> viewSignupSlot rowSignupSlot model) rowSignupSlots


viewSignupSlot : SignupSlot -> Model -> Table.Cell Msg
viewSignupSlot signupSlot model =
    let
        isFocused =
            model.focusedSlotId == Just signupSlot.id
    in
        Table.td []
            [ div [ class "signups" ] (viewSignupsForSlot signupSlot model.signups)
            , viewFocusedSignupForm signupSlot model isFocused
            , viewSignupSlotJoinButton signupSlot model isFocused
            ]


viewFocusedSignupForm : SignupSlot -> Model -> Bool -> Html Msg
viewFocusedSignupForm signupSlot model isFocused =
    (if isFocused then
        div [ class "signup-form" ]
            [ form [ onSubmit SubmitNewSignup ]
                [ div [] [ input [ type_ "text", name "name", placeholder "Name", autofocus True, onInput EditNewSignupName, required True ] [] ]
                , div [] [ input [ type_ "email", name "email", placeholder "Email", onInput EditNewSignupEmail, required True ] [] ]
                , div [] [ textarea [ name "comment", placeholder "Comment (optional)", onInput EditNewSignupComment ] [] ]
                , div []
                    [ Button.button [ Button.small, Button.primary, Button.attrs [ type_ "submit" ] ] [ text "Sign up" ]
                    , div [] [ text "or" ]
                    , a [ href "javascript:void(0);", onClick (CancelSlotFocus signupSlot.id) ] [ text "cancel" ]
                    ]
                ]
            ]
     else
        div [] []
    )


viewSignupSlotJoinButton : SignupSlot -> Model -> Bool -> Html Msg
viewSignupSlotJoinButton signupSlot model isFocused =
    (if signupSlot.closed then
        a [ class "btn btn-secondary btn-sm disabled", href "#", disabled True ] [ text "Join →" ]
     else
        (if isFocused then
            div [] []
         else
            Button.button [ Button.small, Button.outlinePrimary, Button.onClick (FocusSlotJoin signupSlot.id) ] [ text "Join →" ]
        )
    )


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
    let
        signupText =
            (if String.isEmpty signup.comment then
                signup.name
             else
                (signup.name ++ " (" ++ signup.comment ++ ")")
            )
    in
        div [] [ text signupText ]


signupsForSlot : SignupSlot -> List Signup -> List Signup
signupsForSlot signupSlot signupList =
    List.filter (\signup -> signup.signupSlotId == signupSlot.id) signupList



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


encodedSignupValue : Model -> Json.Encode.Value
encodedSignupValue model =
    Json.Encode.object
        [ ( "signup_slot_id", Json.Encode.string (Maybe.withDefault "" model.focusedSlotId) )
        , ( "name", Json.Encode.string (Maybe.withDefault "" model.currentNewSignupName) )
        , ( "email", Json.Encode.string (Maybe.withDefault "" model.currentNewSignupEmail) )
        , ( "comment", Json.Encode.string (Maybe.withDefault "" model.currentNewSignupComment) )
        ]


postNewSignup : Model -> Http.Request SignupJSONResponse
postNewSignup model =
    Http.post "//localhost:3000/api/v1/signups"
        (Http.jsonBody (encodedSignupValue model))
        decodeSignupResponse


createSignup : Model -> Cmd Msg
createSignup model =
    Http.send ReceiveSignupResponse (postNewSignup model)


getSheetDetails : String -> Cmd Msg
getSheetDetails sheetId =
    let
        url =
            "//localhost:3000/api/v1/sheets/" ++ sheetId ++ ".json"
    in
        log url
            Http.send
            ReceiveSheetDetails
            (Http.get url decodeSheetResponse)


decodeSignupResponse : Decoder SignupJSONResponse
decodeSignupResponse =
    map4 SignupJSONResponse
        (field "id" string)
        (field "signup_slot_id" string)
        (field "name" string)
        (field "comment" string)


decodeSheetResponse : Decoder SheetJSONResponse
decodeSheetResponse =
    map7 SheetJSONResponse
        (field "id" string)
        (field "title" string)
        (field "description" string)
        (field "rows" decodeRows)
        (field "columns" decodeColumns)
        (field "signup_slots" decodeSignupSlots)
        (field "signups" decodeSignups)
