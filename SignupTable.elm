module SignupTable exposing (Msg(..), Model, init, update, view, subscriptions)

import Data.Sheet exposing (Column, Row, SheetJSONResponse, Signup, SignupJSONResponse, SignupSlot, decodeColumns, decodeRows, decodeSignupSlots, decodeSignups)
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, classList, disabled, for, href, name, placeholder, required, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, field, string, map4, map7)
import Json.Encode


-- Bootstrap style helpers

import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Table as Table
import Bootstrap.Popover as Popover


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
    | PopoverMsg String Popover.State


type alias SheetID =
    String


type alias RowID =
    String


type alias SignupSlotID =
    String


type alias SignupID =
    String


type alias SignupSlotPopover =
    { signupSlotId : SignupSlotID
    , popoverState : Popover.State
    }


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
    , signupSlotPopovers : List SignupSlotPopover
    }


type alias Sortable a =
    { a | position : Int }



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
        []
    , getSheetDetails sheetId
    )



-- UPDATE


initializeSignupSlotPopovers : List SignupSlot -> List SignupSlotPopover
initializeSignupSlotPopovers signupSlotList =
    List.map initializeSignupSlotPopover signupSlotList


initializeSignupSlotPopover : SignupSlot -> SignupSlotPopover
initializeSignupSlotPopover signupSlot =
    { signupSlotId = signupSlot.id, popoverState = Popover.initialState }


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
                (initializeSignupSlotPopovers jsonResponse.signupSlots)
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

        CancelSlotFocus slotId ->
            ( defocusSlot model
            , Cmd.none
            )

        PopoverMsg slotId state ->
            let
                updatedSignupSlotPopovers =
                    updateStateForPopoverInSlot state slotId model.signupSlotPopovers
            in
                { model | signupSlotPopovers = updatedSignupSlotPopovers }
                    |> update (FocusSlotJoin slotId)


updateStateForPopoverInSlot : Popover.State -> SignupSlotID -> List SignupSlotPopover -> List SignupSlotPopover
updateStateForPopoverInSlot popoverState signupSlotId signupSlotPopoverList =
    List.map
        (\slotPopover ->
            (if slotPopover.signupSlotId == signupSlotId then
                { slotPopover | popoverState = popoverState }
             else
                { slotPopover | popoverState = Popover.initialState }
            )
        )
        signupSlotPopoverList


defocusSlot : Model -> Model
defocusSlot model =
    { model
        | focusedSlotId = Nothing
        , currentNewSignupName = Nothing
        , currentNewSignupEmail = Nothing
        , currentNewSignupComment = Nothing
        , signupSlotPopovers = initializeSignupSlotPopovers model.signupSlots
    }



--
-- VIEW


view : Model -> Html Msg
view model =
    Grid.containerFluid []
        [ Grid.row []
            [ Grid.col []
                [ viewDevelopmentDebugHeader model
                , div [ class "sheet" ]
                    [ h1 [ class "sheet__title" ] [ text (model.title) ]
                    , p [ class "sheet__description" ] [ text (model.description) ]
                    , viewTable model
                    ]
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
    Table.table
        { options = [ Table.attr <| class "signup-table" ]
        , thead = Table.thead [] [ viewTableColumnHeaderRow model.columns ]
        , tbody = Table.tbody [] (List.map (viewTableRow model) model.rows)
        }


sortByPosition : List (Sortable a) -> List (Sortable a)
sortByPosition sortable =
    List.sortBy .position sortable


viewTableColumnHeaderRow : List Column -> Table.Row Msg
viewTableColumnHeaderRow columnList =
    Table.tr [] ([ (Table.th [] [ text "" ]) ] ++ (List.map viewColumnHeader (columnList |> sortByPosition)))


viewColumnHeader : Column -> Table.Cell Msg
viewColumnHeader column =
    Table.th [ Table.cellAttr <| class "signup-table__column-header signup-table__header" ]
        [ text column.value ]


viewTableRow : Model -> Row -> Table.Row Msg
viewTableRow model row =
    let
        signupSlotList =
            model.signupSlots

        signupList =
            model.signups
    in
        Table.tr []
            (List.append
                [ (Table.th [ Table.cellAttr <| class "signup-table__row-header signup-table__header" ] [ text row.value ]) ]
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
        Table.td [ Table.cellAttr <| class "signup-table__cell" ]
            [ div [ class "signup-cell__signup-list" ]
                (List.append
                    (viewSignupsForSlot signupSlot model.signups)
                    (focusedSignupSlot model isFocused)
                )
            , viewSignupForm signupSlot model isFocused
            ]


focusedSignupSlot : Model -> Bool -> List (Html Msg)
focusedSignupSlot model isFocused =
    [ div [] [] ]


popoverStateForSignupSlot : Model -> SignupSlot -> Popover.State
popoverStateForSignupSlot model signupSlot =
    List.filter (\ssp -> ssp.signupSlotId == signupSlot.id) model.signupSlotPopovers
        |> List.head
        |> Maybe.map .popoverState
        |> Maybe.withDefault Popover.initialState


viewSignupForm : SignupSlot -> Model -> Bool -> Html Msg
viewSignupForm signupSlot model isFocused =
    div []
        [ Popover.config
            (Button.button
                [ Button.small
                , Button.outlinePrimary
                , Button.block
                , Button.disabled signupSlot.closed
                , Button.onClick (FocusSlotJoin signupSlot.id)
                , Button.attrs <|
                    Popover.onClick
                        (popoverStateForSignupSlot model signupSlot)
                        (PopoverMsg signupSlot.id)
                ]
                [ text "Join →" ]
            )
            |> Popover.bottom
            |> Popover.titleH4 [] [ text "Join this slot" ]
            |> Popover.content []
                [ div [ classList [ ( "signup-cell__form", True ), ( "signup-cell__form-focus", isFocused ) ] ]
                    [ Form.form [ onSubmit SubmitNewSignup ]
                        [ Form.group []
                            [ Input.text
                                [ Input.onInput EditNewSignupName
                                , Input.attrs [ type_ "text", name "name", placeholder "Name", autofocus True, onInput EditNewSignupName, required True ]
                                ]
                            , Form.help [] [ text "Your name to sign up with" ]
                            ]
                        , Form.group []
                            [ Input.email
                                [ Input.onInput EditNewSignupEmail
                                , Input.attrs [ type_ "email", name "email", placeholder "Email", required True ]
                                ]
                            , Form.help [] [ text "You will be emailed a confirmation to this address." ]
                            ]
                        , Form.group []
                            [ Textarea.textarea
                                [ Textarea.onInput EditNewSignupComment
                                , Textarea.attrs [ name "comment", placeholder "Comment (optional)" ]
                                ]
                            , Form.help [] [ text "Anything other information you want to include" ]
                            ]
                        ]
                    , div []
                        [ Button.button [ Button.primary, Button.attrs [ type_ "submit" ] ] [ text "Sign up" ]
                        , span [] [ text " or " ]
                        , a [ href "javascript:void(0);", onClick (CancelSlotFocus signupSlot.id) ] [ text "cancel" ]
                        ]
                    ]
                ]
            |> Popover.view (popoverStateForSignupSlot model signupSlot)
        ]


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
        div [ class "signup-table__signup" ] [ text signupText ]


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
