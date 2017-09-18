port module Page.Sheet exposing (Msg(..), Model, Flags, init, update, view, subscriptions)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Modal as Modal
import Bootstrap.Progress as Progress
import Data.Sheet exposing (Column, Row, Sheet, Signup, Signup, SignupSlot, SheetID, RowID, SignupSlotID, SignupID, decodeColumns, decodeRows, decodeSignupSlots, decodeSignups)
import Html exposing (..)
import Html.Attributes exposing (autocomplete, autofocus, class, classList, disabled, for, href, name, novalidate, placeholder, required, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Toasty
import Request.Sheet exposing (getSheetDetails, createSignup)
import Scroll exposing (Move)


-- PORTS
-- Outbound: notify the JS world the sheet has changed


port sheetUpdated : Bool -> Cmd msg


port modalOpened : Bool -> Cmd msg



-- Inbound: if a side scroller widget is needed


port needsRightScrollerArrow : (Bool -> msg) -> Sub msg



-- Inbound: If the page is scrolled


port scroll : (Move -> msg) -> Sub msg



-- Inbound: Measure the top height offset of the header


port updateHeaderBoundaries : (Int -> msg) -> Sub msg



-- MESSAGES


type Msg
    = FetchSheet
    | ReceiveSheetDetails (Result Http.Error Sheet)
    | ReceiveSignupResponse (Result Http.Error Signup)
    | ChangeSheetID SheetID
    | FocusSlotJoin SignupSlotID
    | CancelSlotFocus SignupSlotID
    | EditNewSignupName String
    | EditNewSignupEmail String
    | EditNewSignupComment String
    | SubmitNewSignup
    | ModalMsg SignupSlotID Modal.State
    | ChangeFocusedColumn Column
    | ChangeViewStyle PageViewStyle
    | ToastyMsg (Toasty.Msg String)
    | ReceiveNeedsRightScrollerArrowUpdate Bool
    | Header Move
    | HeaderPositionChanged HeaderPositionStyle
    | UpdateHeaderYBoundary Int


type alias SignupSlotModal =
    { signupSlotId : SignupSlotID
    , modalState : Modal.State
    }


type PageViewStyle
    = CardView
    | TableView


type HeaderPositionStyle
    = Fixed
    | Static


type alias Model =
    { sheetId : SheetID
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
    , signupSlotModals : List SignupSlotModal
    , focusedColumn : Maybe Column
    , viewStyle : PageViewStyle
    , isSheetLoading : Bool
    , isSheetError : Bool
    , isProductionMode : Bool
    , apiKey : String
    , toasties : Toasty.Stack String
    , needsRightScrollerArrow : Bool
    , isNameVisible : Bool
    , headerPositionStyle : HeaderPositionStyle
    , headerYBoundary : Int
    }


type alias Sortable a =
    { a | position : Int }


type alias Flags =
    { sheetId : SheetID
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
        Nothing
        CardView
        True
        False
        flags.productionMode
        flags.apiKey
        Toasty.initialState
        False
        True
        Static
        400
    , getSheetDetails flags.apiBaseEndpoint flags.sheetId flags.apiKey
        |> Http.send ReceiveSheetDetails
    )


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
            ( { model | isSheetLoading = True }
            , getSheetDetails model.apiBaseEndpoint model.sheetId model.apiKey |> Http.send ReceiveSheetDetails
            )

        SubmitNewSignup ->
            ( model
            , createSignup
                model.apiBaseEndpoint
                model.apiKey
                model.focusedSlotId
                model.currentNewSignupName
                model.currentNewSignupEmail
                model.currentNewSignupComment
                |> Http.send ReceiveSignupResponse
            )

        ReceiveSignupResponse (Err err) ->
            ( model, Cmd.none )
                |> Toasty.addToast defaultToastConfig ToastyMsg ("We've encountered an error with your signup. Please check your signup fields and try again later.")

        ReceiveSignupResponse (Ok jsonResponse) ->
            let
                focusedSignupSlotMaybe =
                    (getSignupSlot model model.focusedSlotId)
            in
                case focusedSignupSlotMaybe of
                    Just focusedSignupSlot ->
                        model
                            |> defocusSlot
                            |> Toasty.addToast defaultToastConfig ToastyMsg ("You've signed up for " ++ (viewSignupSlotTitle model focusedSignupSlot) ++ ". A confirmation email has been sent to your email address.")
                            |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, getSheetDetails model.apiBaseEndpoint model.sheetId model.apiKey |> Http.send ReceiveSheetDetails ])

                    Nothing ->
                        ( model, Cmd.none )

        ReceiveSheetDetails (Ok jsonResponse) ->
            ( { model
                | title = jsonResponse.title
                , description = jsonResponse.description
                , rows = jsonResponse.rows
                , columns = jsonResponse.columns
                , signupSlots = jsonResponse.signupSlots
                , signups = jsonResponse.signups
                , signupSlotModals = (initializeSignupSlotModals jsonResponse.signupSlots)
                , focusedColumn =
                    (if model.focusedColumn == Nothing then
                        List.head jsonResponse.columns
                     else
                        model.focusedColumn
                    )
                , isSheetLoading = False
                , isSheetError = False
                , isNameVisible = jsonResponse.isNameVisible
              }
            , sheetUpdated True
            )

        ReceiveSheetDetails (Err err) ->
            ( { model | isSheetLoading = False, isSheetError = True }, Cmd.none )

        ReceiveNeedsRightScrollerArrowUpdate needsRightScroller ->
            ( { model | needsRightScrollerArrow = needsRightScroller }, Cmd.none )

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
            defocusSlot model

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

        Header move ->
            Scroll.handle
                [ update (HeaderPositionChanged Fixed) |> Scroll.onCrossDown (toFloat model.headerYBoundary)
                , update (HeaderPositionChanged Static) |> Scroll.onCrossUp (toFloat model.headerYBoundary)
                ]
                move
                model

        HeaderPositionChanged positionStyle ->
            ( { model | headerPositionStyle = positionStyle }, Cmd.none )

        UpdateHeaderYBoundary yOffset ->
            ( { model | headerYBoundary = yOffset }, Cmd.none )


getSignupSlot : Model -> Maybe SignupSlotID -> Maybe SignupSlot
getSignupSlot model signupSlotIdMaybe =
    case signupSlotIdMaybe of
        Just signupSlotId ->
            List.filter (\slot -> slot.id == signupSlotId) model.signupSlots
                |> List.head

        Nothing ->
            Nothing


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
            Html.text ""
          )
        , div [ class "sheet" ]
            (if (isSheetError model) then
                [ Alert.danger [ text "Uh oh! We are having difficulty accessing your sheet. We've been notified - please try again later." ] ]
             else if isSheetLoading model then
                [ div []
                    [ Progress.progress [ Progress.value 100, Progress.height 50, Progress.animated, Progress.striped ]
                    ]
                ]
             else
                [ h1 [ class "sheet__title" ] [ text (model.title) ]
                , p [ class "sheet__description" ] [ text (model.description) ]
                , viewCard model
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


viewCard : Model -> Html Msg
viewCard model =
    div []
        [ viewColumnSideScroller model
        , viewCardList model
        ]


viewHeaderPositionStyle : Model -> String
viewHeaderPositionStyle model =
    if model.headerPositionStyle == Fixed then
        "fixed"
    else
        "relative"


viewColumnSideScroller : Model -> Html Msg
viewColumnSideScroller model =
    div []
        [ (if model.headerPositionStyle == Fixed then
            div [ style [ ( "height", "100px" ) ] ] []
           else
            Html.text ""
          )
        , div
            [ class "side-scroller"
            , style
                [ ( "position", (viewHeaderPositionStyle model) )
                , ( "top", "0" )
                , ( "z-index", "1" )
                ]
            ]
            [ div [ class "side-scroller__items" ] (viewColumnsSideScrollerItems model)
            , (if model.needsRightScrollerArrow then
                div [ class "fab-button fab-button--right fab-button--bounce fab-button--noclick" ] [ i [ class "material-icons" ] [ text ">" ] ]
               else
                Html.text ""
              )
            ]
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
                Html.text ""


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


sortByPosition : List (Sortable a) -> List (Sortable a)
sortByPosition sortable =
    List.sortBy .position sortable


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
                    (viewSignupsForSlot model signupSlot model.signups)
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
                model
                (Signup ""
                    (Maybe.withDefault "" model.currentNewSignupEmail)
                    (Maybe.withDefault "" model.currentNewSignupName)
                    (Maybe.withDefault "" model.currentNewSignupComment)
                )
        else
            Html.text ""


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
            [ Form.group
                []
                [ Input.text
                    [ Input.onInput EditNewSignupName
                    , Input.value <| Maybe.withDefault "" model.currentNewSignupName
                    , Input.attrs [ type_ "text", name "name", placeholder "Name", autofocus True, autocomplete False, required True ]
                    ]
                , Form.help [] [ text "Your name to sign up with" ]
                ]
            , Form.group []
                [ Input.email
                    [ Input.onInput EditNewSignupEmail
                    , Input.value <| Maybe.withDefault "" model.currentNewSignupEmail
                    , Input.attrs [ type_ "email", name "email", placeholder "Email", autocomplete False, required True ]
                    ]
                , Form.help [] [ text "You will be emailed a confirmation to this address." ]
                ]
            , Form.group []
                [ Textarea.textarea
                    [ Textarea.onInput EditNewSignupComment
                    , Textarea.value <| Maybe.withDefault "" model.currentNewSignupComment
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


viewSignupsForSlot : Model -> SignupSlot -> List Signup -> List (Html Msg)
viewSignupsForSlot model signupSlot signupList =
    let
        filteredSignups =
            signupsForSlot signupSlot signupList
    in
        List.map (viewSignup model) filteredSignups


viewSignup : Model -> Signup -> Html Msg
viewSignup model signup =
    let
        signupText =
            (if not model.isNameVisible then
                "Name withheld"
             else if String.isEmpty signup.comment then
                signup.name
             else
                (signup.name ++ " (" ++ signup.comment ++ ")")
            )
    in
        div
            [ classList
                [ ( "signup-list__signup", True )
                , ( "signup-list__signup--private", not model.isNameVisible )
                ]
            ]
            [ text signupText ]


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
    Sub.batch
        [ needsRightScrollerArrow ReceiveNeedsRightScrollerArrowUpdate
        , scroll Header
        , updateHeaderBoundaries UpdateHeaderYBoundary
        ]
