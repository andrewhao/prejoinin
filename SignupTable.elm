port module SignupTable exposing (Msg(..), Model, Flags, init, update, view, subscriptions)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Modal as Modal
import Bootstrap.Popover as Popover
import Bootstrap.Progress as Progress
import Bootstrap.Table as Table
import Data.Sheet exposing (Column, Row, SheetJSONResponse, Signup, SignupJSONResponse, SignupSlot, decodeColumns, decodeRows, decodeSignupSlots, decodeSignups)
import Html exposing (..)
import Html.Attributes exposing (autocomplete, autofocus, class, classList, disabled, for, href, name, placeholder, required, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import HttpBuilder
import Json.Decode exposing (Decoder, field, map4, map7, string)
import Json.Encode
import Toasty


-- PORTS
-- Outbound: notify the JS world the sheet has changed


port sheetUpdated : Bool -> Cmd msg


port modalOpened : Bool -> Cmd msg



-- Inbound: if a side scroller widget is needed


port needsRightScrollerArrow : (Bool -> msg) -> Sub msg



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
    | PopoverMsg SignupSlotID Popover.State
    | ModalMsg SignupSlotID Modal.State
    | ChangeFocusedColumn Column
    | ChangeViewStyle PageViewStyle
    | ToastyMsg (Toasty.Msg String)
    | ReceiveNeedsRightScrollerArrowUpdate Bool


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


type alias SignupSlotModal =
    { signupSlotId : SignupSlotID
    , modalState : Modal.State
    }


type PageViewStyle
    = CardView
    | TableView


type alias Model =
    { sheetId : Maybe SheetID
    , apiBaseEndpoint : String
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
    , signupSlotModals : List SignupSlotModal
    , focusedColumn : Maybe Column
    , viewStyle : PageViewStyle
    , isSheetLoading : Bool
    , isSheetError : Bool
    , isProductionMode : Bool
    , apiKey : String
    , toasties : Toasty.Stack String
    , needsRightScrollerArrow : Bool
    }


type alias Sortable a =
    { a | position : Int }


type alias Flags =
    { sheetId : Maybe SheetID
    , apiBaseEndpoint : String
    , productionMode : Bool
    , apiKey : String
    }



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model flags.sheetId
        flags.apiBaseEndpoint
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
        []
        Nothing
        CardView
        True
        False
        flags.productionMode
        flags.apiKey
        Toasty.initialState
        False
    , getSheetDetails flags.apiBaseEndpoint flags.sheetId flags.apiKey
    )


initializeSignupSlotPopovers : List SignupSlot -> List SignupSlotPopover
initializeSignupSlotPopovers signupSlotList =
    List.map initializeSignupSlotPopover signupSlotList


initializeSignupSlotPopover : SignupSlot -> SignupSlotPopover
initializeSignupSlotPopover signupSlot =
    { signupSlotId = signupSlot.id, popoverState = Popover.initialState }


initializeSignupSlotModals : List SignupSlot -> List SignupSlotModal
initializeSignupSlotModals signupSlotList =
    List.map initializeSignupSlotModal signupSlotList


initializeSignupSlotModal : SignupSlot -> SignupSlotModal
initializeSignupSlotModal signupSlot =
    { signupSlotId = signupSlot.id, modalState = Modal.hiddenState }


defaultToastConfig : Toasty.Config msg
defaultToastConfig =
    Toasty.config
        |> Toasty.transitionOutDuration 800
        |> Toasty.transitionInAttrs [ class "notification__container notification__container--fade-in" ]
        |> Toasty.transitionOutAttrs [ class "notification__container--fade-out" ]
        |> Toasty.containerAttrs [ class "notification" ]
        |> Toasty.delay 8000



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeViewStyle viewStyle ->
            ( { model | viewStyle = viewStyle }, Cmd.none )

        ChangeFocusedColumn column ->
            ( { model | focusedColumn = Just column }, Cmd.none )

        FetchSheet ->
            ( { model | isSheetLoading = True }, getSheetDetails model.apiBaseEndpoint model.sheetId model.apiKey )

        SubmitNewSignup ->
            let
                focusedSignupSlotMaybe =
                    (getSignupSlot model model.focusedSlotId)
            in
                case focusedSignupSlotMaybe of
                    Just focusedSignupSlot ->
                        ( model, createSignup model )
                            |> Toasty.addToast defaultToastConfig ToastyMsg ("You've signed up for " ++ (viewSignupSlotTitle model focusedSignupSlot) ++ ". A confirmation email has been sent to your email address.")

                    Nothing ->
                        ( model, Cmd.none )

        ReceiveSignupResponse (Err err) ->
            ( model, Cmd.none )

        ReceiveSignupResponse (Ok jsonResponse) ->
            model
                |> defocusSlot
                |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, getSheetDetails model.apiBaseEndpoint model.sheetId model.apiKey ])

        ReceiveSheetDetails (Ok jsonResponse) ->
            ( { model
                | title = jsonResponse.title
                , description = jsonResponse.description
                , rows = jsonResponse.rows
                , columns = jsonResponse.columns
                , signupSlots = jsonResponse.signupSlots
                , signups = jsonResponse.signups
                , signupSlotPopovers = (initializeSignupSlotPopovers jsonResponse.signupSlots)
                , signupSlotModals = (initializeSignupSlotModals jsonResponse.signupSlots)
                , focusedColumn =
                    (if model.focusedColumn == Nothing then
                        List.head jsonResponse.columns
                     else
                        model.focusedColumn
                    )
                , isSheetLoading = False
                , isSheetError = False
              }
            , sheetUpdated True
            )

        ReceiveSheetDetails (Err err) ->
            ( { model | isSheetLoading = False, isSheetError = True }, Cmd.none )

        ReceiveNeedsRightScrollerArrowUpdate needsRightScroller ->
            ( { model | needsRightScrollerArrow = needsRightScroller }, Cmd.none )

        ChangeSheetID newSheetId ->
            ( { model | sheetId = Just newSheetId }
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
            defocusSlot model

        PopoverMsg slotId state ->
            let
                updatedSignupSlotPopovers =
                    updateStateForPopoverInSlot state slotId model.signupSlotPopovers
            in
                { model | signupSlotPopovers = updatedSignupSlotPopovers }
                    |> update (FocusSlotJoin slotId)

        ModalMsg slotId state ->
            let
                updatedSignupSlotModals =
                    updateStateForModalInSlot state slotId model.signupSlotModals
            in
                { model | signupSlotModals = updatedSignupSlotModals }
                    |> update (FocusSlotJoin slotId)
                    |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, (modalOpened (Modal.visibleState == state)) ])

        ToastyMsg subMsg ->
            Toasty.update defaultToastConfig ToastyMsg subMsg model


getSignupSlot : Model -> Maybe SignupSlotID -> Maybe SignupSlot
getSignupSlot model signupSlotIdMaybe =
    case signupSlotIdMaybe of
        Just signupSlotId ->
            List.filter (\slot -> slot.id == signupSlotId) model.signupSlots
                |> List.head

        Nothing ->
            Nothing


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


updateStateForModalInSlot : Modal.State -> SignupSlotID -> List SignupSlotModal -> List SignupSlotModal
updateStateForModalInSlot modalState signupSlotId signupSlotModalList =
    List.map
        (\slotModal ->
            (if slotModal.signupSlotId == signupSlotId then
                { slotModal | modalState = modalState }
             else
                { slotModal | modalState = Modal.hiddenState }
            )
        )
        signupSlotModalList


defocusSlot : Model -> ( Model, Cmd Msg )
defocusSlot model =
    ( { model
        | focusedSlotId = Nothing
        , currentNewSignupName = Nothing
        , currentNewSignupEmail = Nothing
        , currentNewSignupComment = Nothing
        , signupSlotPopovers = initializeSignupSlotPopovers model.signupSlots
        , signupSlotModals = initializeSignupSlotModals model.signupSlots
      }
    , (modalOpened False)
    )



--
-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ (if not model.isProductionMode then
            viewDevelopmentDebugHeader model
           else
            div [] []
          )
        , div [ class "sheet" ]
            (if (not (isSheetDefined model)) || (isSheetError model) then
                [ Alert.danger [ text "Uh oh! We are having difficulty accessing your sheet. We've been notified - please try again later." ] ]
             else if isSheetLoading model then
                [ div []
                    [ Progress.progress [ Progress.value 100, Progress.height 50, Progress.animated, Progress.striped ]
                    ]
                ]
             else
                [ h1 [ class "sheet__title" ] [ text (model.title) ]
                , p [ class "sheet__description" ] [ text (model.description) ]
                , if model.viewStyle == CardView then
                    viewCard model
                  else
                    viewTable model
                ]
            )
        , Toasty.view defaultToastConfig renderToast ToastyMsg model.toasties
        ]


renderToast : String -> Html Msg
renderToast toast =
    div [ class "notification__toast" ] [ text toast ]


isSheetError : Model -> Bool
isSheetError model =
    model.isSheetError


isSheetLoading : Model -> Bool
isSheetLoading model =
    model.isSheetLoading == True


isSheetDefined : Model -> Bool
isSheetDefined model =
    model.sheetId /= Nothing


viewCard : Model -> Html Msg
viewCard model =
    div []
        [ viewColumnSideScroller model
        , viewCardList model
        ]


viewColumnSideScroller : Model -> Html Msg
viewColumnSideScroller model =
    div [ class "side-scroller" ]
        [ div [ class "side-scroller__items" ] (viewColumnsSideScrollerItems model)
        , (if model.needsRightScrollerArrow then
            div [ class "fab-button fab-button--right fab-button--bounce fab-button--noclick" ] [ i [ class "material-icons" ] [ text "arrow_forward" ] ]
           else
            Html.text ""
          )
        ]


viewColumnsSideScrollerItems : Model -> List (Html Msg)
viewColumnsSideScrollerItems model =
    List.map (viewColumnSideScrollerItem model) model.columns


viewColumnSideScrollerItem : Model -> Column -> Html Msg
viewColumnSideScrollerItem model column =
    let
        isFocused =
            (model.focusedColumn == Just column)
    in
        div
            [ onClick (ChangeFocusedColumn column)
            , classList
                [ ( "side-scroller__tab", True )
                , ( "side-scroller__tab--active", isFocused )
                ]
            ]
            [ text column.value
            ]


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
                Card.config [ Card.attrs [ class "signup-card-list__card" ] ]
                    |> Card.header [ class "signup-card-list__row-header" ] [ text row.value ]
                    |> Card.block [ Card.blockAttrs [ class "signup-card-list__card-body" ] ]
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


getColumnForSlot : SignupSlot -> List Column -> Maybe Column
getColumnForSlot signupSlot columnList =
    columnList
        |> List.filter (\column -> column.id == signupSlot.columnId)
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
                            , Input.text [ Input.id "dev_sheet_id", Input.onInput ChangeSheetID, Input.value (Maybe.withDefault "" model.sheetId) ]
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
            [ div [ class "signup-list" ]
                (List.append
                    (viewSignupsForSlot signupSlot model.signups)
                    [ focusedSignupSlot model isFocused ]
                )
            , if (isSignupSlotFull model signupSlot) then
                Button.button
                    [ Button.disabled True
                    , Button.small
                    , Button.outlineSecondary
                    , Button.block
                    ]
                    [ text "Slot full" ]
              else if (isSignupSlotClosed signupSlot) then
                Button.button
                    [ Button.disabled True
                    , Button.small
                    , Button.outlineSecondary
                    , Button.block
                    ]
                    [ text "Slot closed" ]
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
            [ div [ class "signup-list" ]
                (List.append
                    (viewSignupsForSlot signupSlot model.signups)
                    [ focusedSignupSlot model isFocused ]
                )
            , if (isSignupSlotFull model signupSlot) then
                Button.button
                    [ Button.disabled True
                    , Button.small
                    , Button.outlineSecondary
                    ]
                    [ text "Slot full" ]
              else if (isSignupSlotClosed signupSlot) then
                Button.button
                    [ Button.disabled True
                    , Button.small
                    , Button.outlineSecondary
                    ]
                    [ text "Slot closed" ]
              else
                viewSignupFormAsModal signupSlot model isFocused
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
        if isFocused && (not allSignupInputBlank) then
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


modalStateForSignupSlot : Model -> SignupSlot -> Modal.State
modalStateForSignupSlot model signupSlot =
    List.filter (\ssp -> ssp.signupSlotId == signupSlot.id) model.signupSlotModals
        |> List.head
        |> Maybe.map .modalState
        |> Maybe.withDefault Modal.hiddenState


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
                , Button.primary
                , Button.onClick (FocusSlotJoin signupSlot.id)
                , Button.attrs <|
                    Popover.onClick
                        (popoverStateForSignupSlot model signupSlot)
                        (PopoverMsg signupSlot.id)
                ]
                [ text "Join →" ]
            )
            |> Popover.bottom
            |> Popover.titleH3 [] [ text ("Join this slot: " ++ (viewSignupSlotTitle model signupSlot)) ]
            |> Popover.content []
                [ viewSignupRawForm signupSlot model isFocused
                ]
            |> Popover.view (popoverStateForSignupSlot model signupSlot)
        ]


viewSignupFormAsModal : SignupSlot -> Model -> Bool -> Html Msg
viewSignupFormAsModal signupSlot model isFocused =
    div []
        [ (Button.button
            [ Button.small
            , Button.outlineSecondary
            , Button.onClick (FocusSlotJoin signupSlot.id)
            , Button.attrs
                [ onClick <| ModalMsg signupSlot.id Modal.visibleState
                , class "signup-card-list__action"
                ]
            ]
            [ text "Join →" ]
          )
        , Modal.config (ModalMsg signupSlot.id)
            |> Modal.large
            |> Modal.header [ class "signup-modal__header" ]
                [ text ("Join this slot: " ++ (viewSignupSlotTitle model signupSlot)) ]
            |> Modal.body [ class "signup-modal__body" ]
                [ viewSignupRawForm signupSlot model isFocused
                ]
            |> Modal.view (modalStateForSignupSlot model signupSlot)
        ]


viewSignupRawForm : SignupSlot -> Model -> Bool -> Html Msg
viewSignupRawForm signupSlot model isFocused =
    div [ classList [ ( "signup-cell__form", True ), ( "signup-cell__form-focus", isFocused ) ] ]
        [ Form.form [ onSubmit SubmitNewSignup ]
            [ Form.group []
                [ Input.text
                    [ Input.onInput EditNewSignupName
                    , Input.attrs [ type_ "text", name "name", placeholder "Name", autofocus True, autocomplete False, onInput EditNewSignupName, required True ]
                    ]
                , Form.help [] [ text "Your name to sign up with" ]
                ]
            , Form.group []
                [ Input.email
                    [ Input.onInput EditNewSignupEmail
                    , Input.attrs [ type_ "email", name "email", placeholder "Email", autocomplete False, required True ]
                    ]
                , Form.help [] [ text "You will be emailed a confirmation to this address." ]
                ]
            , Form.group []
                [ Textarea.textarea
                    [ Textarea.onInput EditNewSignupComment
                    , Textarea.attrs [ name "comment", autocomplete False, placeholder "Comment (optional)" ]
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
        div [ class "signup-list__signup" ] [ text signupText ]


viewSignupSlotTitle : Model -> SignupSlot -> String
viewSignupSlotTitle model signupSlot =
    [ getColumnForSlot signupSlot model.columns, getRowForSlot signupSlot model.rows ]
        |> List.filter (\headerMaybe -> headerMaybe /= Nothing)
        |> List.map (Maybe.map .value)
        |> List.map (Maybe.withDefault "")
        |> String.join ", "


signupsForSlot : SignupSlot -> List Signup -> List Signup
signupsForSlot signupSlot signupList =
    List.filter (\signup -> signup.signupSlotId == signupSlot.id) signupList



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    needsRightScrollerArrow ReceiveNeedsRightScrollerArrowUpdate



-- HTTP


encodedSignupValue : Model -> Json.Encode.Value
encodedSignupValue model =
    Json.Encode.object
        [ ( "signup_slot_id", Json.Encode.string (Maybe.withDefault "" model.focusedSlotId) )
        , ( "name", Json.Encode.string (Maybe.withDefault "" model.currentNewSignupName) )
        , ( "email", Json.Encode.string (Maybe.withDefault "" model.currentNewSignupEmail) )
        , ( "comment", Json.Encode.string (Maybe.withDefault "" model.currentNewSignupComment) )
        ]


createSignup : Model -> Cmd Msg
createSignup model =
    let
        url =
            model.apiBaseEndpoint ++ "/api/v1/signups"
    in
        HttpBuilder.post url
            |> HttpBuilder.withHeaders [ ( "Content-Type", "application/json" ), ( "Authorization", "WejoininAuth key=" ++ model.apiKey ) ]
            |> HttpBuilder.withJsonBody (encodedSignupValue model)
            |> HttpBuilder.withExpect (Http.expectJson decodeSignupResponse)
            |> HttpBuilder.send ReceiveSignupResponse


getSheetDetails : String -> Maybe SheetID -> String -> Cmd Msg
getSheetDetails apiBaseEndpoint maybeSheetId apiKey =
    let
        url =
            apiBaseEndpoint ++ "/api/v1/sheets/" ++ (Maybe.withDefault "" maybeSheetId) ++ ".json"
    in
        case maybeSheetId of
            Just sheetId ->
                HttpBuilder.get url
                    |> HttpBuilder.withHeaders [ ( "Content-Type", "application/json" ), ( "Authorization", "WejoininAuth key=" ++ apiKey ) ]
                    |> HttpBuilder.withExpect (Http.expectJson decodeSheetResponse)
                    |> HttpBuilder.send ReceiveSheetDetails

            Nothing ->
                Cmd.none


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
