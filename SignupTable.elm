module SignupTable exposing (Msg(..), Model, init, update, view, subscriptions)

import Data.Sheet exposing (Column, Row, SheetJSONResponse, Signup, SignupJSONResponse, SignupSlot, decodeColumns, decodeRows, decodeSignupSlots, decodeSignups)
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, classList, disabled, for, href, name, placeholder, required, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, field, map4, map7, string)
import Json.Encode


-- Bootstrap style helpers

import Bootstrap.ButtonGroup as ButtonGroup
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
import Bootstrap.Button as Button


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
    | ChangeFocusedColumn Column
    | ChangeViewStyle PageViewStyle


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


type PageViewStyle
    = CardView
    | TableView


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
    , focusedColumn : Maybe Column
    , viewStyle : PageViewStyle
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
        Nothing
        CardView
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
        ChangeViewStyle viewStyle ->
            ( { model | viewStyle = viewStyle }, Cmd.none )

        ChangeFocusedColumn column ->
            ( { model | focusedColumn = Just column }, Cmd.none )

        FetchSheet ->
            ( model, getSheetDetails model.sheetId )

        SubmitNewSignup ->
            Debug.log ("submitting new signup")
                ( model, createSignup model )

        ReceiveSignupResponse (Err err) ->
            log (toString err)
                ( model, Cmd.none )

        ReceiveSignupResponse (Ok jsonResponse) ->
            ( model |> defocusSlot
            , getSheetDetails model.sheetId
            )

        ReceiveSheetDetails (Ok jsonResponse) ->
            ( { model
                | title = jsonResponse.title
                , description = jsonResponse.description
                , rows = jsonResponse.rows
                , columns = jsonResponse.columns
                , signupSlots = jsonResponse.signupSlots
                , signups = jsonResponse.signups
                , signupSlotPopovers = (initializeSignupSlotPopovers jsonResponse.signupSlots)
                , focusedColumn = List.head jsonResponse.columns
              }
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
                    , if model.viewStyle == CardView then
                        viewCard model
                      else
                        viewTable model
                    ]
                ]
            ]
        ]


viewCard : Model -> Html Msg
viewCard model =
    div []
        [ viewColumnSideScroller model
        , viewCardList model
        ]


viewColumnSideScroller : Model -> Html Msg
viewColumnSideScroller model =
    div [ class "side-scroller" ]
        [ viewColumnsSideScrollerItems model ]


viewColumnsSideScrollerItems : Model -> Html Msg
viewColumnsSideScrollerItems model =
    ButtonGroup.radioButtonGroup [ ButtonGroup.large ]
        (List.map (viewColumnSideScrollerItem model) model.columns)


viewColumnSideScrollerItem : Model -> Column -> ButtonGroup.RadioButtonItem Msg
viewColumnSideScrollerItem model column =
    ButtonGroup.radioButton (model.focusedColumn == Just column)
        [ Button.secondary, Button.onClick <| ChangeFocusedColumn column ]
        [ text column.value ]


viewCardList : Model -> Html Msg
viewCardList model =
    let
        signupSlots =
            (signupSlotsForColumn model model.focusedColumn)
    in
        div [ class "signup-card-list" ]
            (List.map (viewCardForSlot model) signupSlots)


viewCardForSlot : Model -> SignupSlot -> Html Msg
viewCardForSlot model signupSlot =
    let
        rowMaybe =
            getRowForSlot signupSlot model.rows
    in
        case rowMaybe of
            Just row ->
                Card.config []
                    |> Card.header [] [ text row.value ]
                    |> Card.block []
                        [ Card.text []
                            [ viewSignupSlotAsCard model signupSlot ]
                        ]
                    |> Card.view

            Nothing ->
                div [] []


signupSlotsForColumn : Model -> Maybe Column -> List SignupSlot
signupSlotsForColumn model selectedColumn =
    case selectedColumn of
        Just column ->
            List.filter (\slot -> slot.columnId == column.id) model.signupSlots
                |> sortSignupSlotsByRowOrder model

        Nothing ->
            []


sortSignupSlotsByRowOrder : Model -> List SignupSlot -> List SignupSlot
sortSignupSlotsByRowOrder model signupSlotList =
    let
        orderedRows =
            sortByPosition model.rows
    in
        List.sortBy (\slot -> (Maybe.map .position (getRowForSlot slot model.rows)) |> Maybe.withDefault 0) signupSlotList


getRowForSlot : SignupSlot -> List Row -> Maybe Row
getRowForSlot signupSlot rowList =
    rowList
        |> List.filter (\row -> row.id == signupSlot.rowId)
        |> List.head


viewDevelopmentDebugHeader : Model -> Html Msg
viewDevelopmentDebugHeader model =
    Card.group
        [ Card.config []
            |> Card.header []
                [ text "Select sheet" ]
            |> Card.block []
                [ Card.text []
                    [ Form.form []
                        [ Form.group []
                            [ Form.label [ for "dev_sheet_id" ] [ text "Sheet ID" ]
                            , Input.text [ Input.id "dev_sheet_id", Input.onInput ChangeSheetID, Input.value model.sheetId ]
                            , Form.help [] [ text "Sheet ID you wish to query for: /sheets/:sheet_id" ]
                            , Button.button [ Button.primary, Button.onClick FetchSheet ] [ text "Fetch" ]
                            ]
                        ]
                    ]
                ]
        , Card.config []
            |> Card.header []
                [ text "View style" ]
            |> Card.block []
                [ Card.text []
                    [ ButtonGroup.radioButtonGroup []
                        [ ButtonGroup.radioButton (model.viewStyle == CardView) [ Button.primary, Button.onClick <| ChangeViewStyle CardView ] [ text "Cards" ]
                        , ButtonGroup.radioButton (model.viewStyle == TableView) [ Button.primary, Button.onClick <| ChangeViewStyle TableView ] [ text "Table" ]
                        ]
                    ]
                ]
        ]


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
        Table.td
            [ Table.cellAttr <|
                classList
                    [ ( "signup-table__cell", True )
                    , ( "signup-table__cell--focused", isFocused )
                    ]
            ]
            [ div [ class "signup-cell__signup-list" ]
                (List.append
                    (viewSignupsForSlot signupSlot model.signups)
                    [ focusedSignupSlot model isFocused ]
                )
            , if (isSignupSlotFull model signupSlot) then
                div [ class "signup-table__cell-info--full signup-table__cell-info" ] [ text "Slot full" ]
              else if (isSignupSlotClosed signupSlot) then
                div [ class "signup-table__cell-info--closed signup-table__cell-info" ] [ text "Slot closed" ]
              else
                viewSignupForm signupSlot model isFocused
            ]


viewSignupSlotAsCard : Model -> SignupSlot -> Html Msg
viewSignupSlotAsCard model signupSlot =
    let
        isFocused =
            model.focusedSlotId == Just signupSlot.id
    in
        div
            [ classList
                [ ( "signup-card", True )
                , ( "signup-card--focused", isFocused )
                ]
            ]
            [ div [ class "signup-card__signup-list" ]
                (List.append
                    (viewSignupsForSlot signupSlot model.signups)
                    [ focusedSignupSlot model isFocused ]
                )
            , if (isSignupSlotFull model signupSlot) then
                div [ class "signup-card__info--full signup-card__info" ] [ text "Slot full" ]
              else if (isSignupSlotClosed signupSlot) then
                div [ class "signup-card__info--closed signup-card__info" ] [ text "Slot closed" ]
              else
                viewSignupForm signupSlot model isFocused
            ]


focusedSignupSlot : Model -> Bool -> Html Msg
focusedSignupSlot model isFocused =
    let
        inputStrings =
            [ model.currentNewSignupName, model.currentNewSignupEmail, model.currentNewSignupComment ]
                |> List.map (Maybe.withDefault "")

        allSignupInputBlank =
            inputStrings
                |> List.all String.isEmpty
    in
        if isFocused then
            viewSignup
                (Signup ""
                    (Maybe.withDefault "" model.currentNewSignupEmail)
                    (Maybe.withDefault "" model.currentNewSignupName)
                    (Maybe.withDefault "" model.currentNewSignupComment)
                )
        else
            div [] []


popoverStateForSignupSlot : Model -> SignupSlot -> Popover.State
popoverStateForSignupSlot model signupSlot =
    List.filter (\ssp -> ssp.signupSlotId == signupSlot.id) model.signupSlotPopovers
        |> List.head
        |> Maybe.map .popoverState
        |> Maybe.withDefault Popover.initialState


isSignupSlotFull : Model -> SignupSlot -> Bool
isSignupSlotFull model signupSlot =
    if signupSlot.maxSignups == 0 then
        False
    else
        signupsForSlot signupSlot model.signups
            |> List.length
            |> (<=) signupSlot.maxSignups


isSignupSlotClosed : SignupSlot -> Bool
isSignupSlotClosed signupSlot =
    signupSlot.closed


viewSignupForm : SignupSlot -> Model -> Bool -> Html Msg
viewSignupForm signupSlot model isFocused =
    div []
        [ Popover.config
            (Button.button
                [ Button.small
                , Button.outlinePrimary
                , Button.block
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
                [ viewSignupRawForm signupSlot model isFocused
                ]
            |> Popover.view (popoverStateForSignupSlot model signupSlot)
        ]


viewSignupRawForm : SignupSlot -> Model -> Bool -> Html Msg
viewSignupRawForm signupSlot model isFocused =
    div [ classList [ ( "signup-cell__form", True ), ( "signup-cell__form-focus", isFocused ) ] ]
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
            , div []
                [ Button.button [ Button.primary, Button.attrs [ type_ "submit" ] ] [ text "Sign up" ]
                , span [] [ text " or " ]
                , a [ href "javascript:void(0);", onClick (CancelSlotFocus signupSlot.id) ] [ text "cancel" ]
                ]
            ]
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
    Http.post "//wejoinin.herokuapp.com/api/v1/signups"
        (Http.jsonBody (encodedSignupValue model))
        decodeSignupResponse


createSignup : Model -> Cmd Msg
createSignup model =
    Http.send ReceiveSignupResponse (postNewSignup model)


getSheetDetails : String -> Cmd Msg
getSheetDetails sheetId =
    let
        url =
            "//wejoinin.herokuapp.com/api/v1/sheets/" ++ sheetId ++ ".json"
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
