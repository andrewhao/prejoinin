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
import Html.Attributes
    exposing
        ( autofocus
        , href
        , class
        , classList
        , disabled
        , name
        , placeholder
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (..)


-- MESSAGES


type Msg
    = FetchSheet
    | ReceiveSheetDetails (Result Http.Error SheetJSONResponse)
    | ChangeSheetID SheetID
    | FocusSlotJoin SignupSlotID
    | EditNewSignupName String
    | EditNewSignupEmail String
    | EditNewSignupComment String
    | CancelSlotFocus SignupSlotID


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
            ( { model
                | focusedSlotId = Nothing
                , currentNewSignupName = Nothing
                , currentNewSignupEmail = Nothing
                , currentNewSignupComment = Nothing
              }
            , Cmd.none
            )



--
-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Sheet ID", onInput ChangeSheetID, Html.Attributes.value model.sheetId ] []
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
            , tbody [] (List.map (\row -> viewTableRow row model) model.rows)
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


viewTableRow : Row -> Model -> Html Msg
viewTableRow row model =
    let
        signupSlotList =
            model.signupSlots

        signupList =
            model.signups
    in
        tr []
            (List.append
                [ (th [] [ text row.value ]) ]
                (viewRowSlots row model)
            )


viewRowSlots : Row -> Model -> List (Html Msg)
viewRowSlots row model =
    let
        signupList =
            model.signups

        rowSignupSlots =
            (List.filter (\slot -> slot.rowId == row.id) model.signupSlots)
    in
        List.map (\rowSignupSlot -> viewSignupSlot rowSignupSlot model) rowSignupSlots


viewSignupSlot : SignupSlot -> Model -> Html Msg
viewSignupSlot signupSlot model =
    let
        isFocused =
            model.focusedSlotId == Just signupSlot.id
    in
        td []
            [ div [ class "signups" ] (viewSignupsForSlot signupSlot model.signups)
            , viewFocusedSignupForm signupSlot model isFocused
            , viewSignupSlotJoinButton signupSlot model isFocused
            ]


viewFocusedSignupForm : SignupSlot -> Model -> Bool -> Html Msg
viewFocusedSignupForm signupSlot model isFocused =
    (if isFocused then
        div [ class "signup-form" ]
            [ form []
                [ div [] [ input [ type_ "text", name "name", placeholder "Name", autofocus True, onInput EditNewSignupName ] [] ]
                , div [] [ input [ type_ "email", name "email", placeholder "Email", onInput EditNewSignupEmail ] [] ]
                , div [] [ textarea [ name "comment", placeholder "Comment", onInput EditNewSignupComment ] [] ]
                ]
            ]
     else
        div [] [ text "" ]
    )


viewSignupSlotJoinButton : SignupSlot -> Model -> Bool -> Html Msg
viewSignupSlotJoinButton signupSlot model isFocused =
    (if signupSlot.closed then
        button [ class "join", disabled True ] [ text "Join ->" ]
     else
        (if isFocused then
            div []
                [ button [ class "submit" ] [ text "Sign up" ]
                , div [] [ text "or" ]
                , a [ href "#", onClick (CancelSlotFocus signupSlot.id) ] [ text "cancel" ]
                ]
         else
            button [ class "join", onClick (FocusSlotJoin signupSlot.id) ] [ text "Join ->" ]
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
