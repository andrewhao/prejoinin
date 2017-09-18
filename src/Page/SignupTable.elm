module Page.SignupTable exposing (Model, Msg, view, update)

import Bootstrap.Popover as Popover
import Html exposing (..)
import Data.Sheet exposing (SignupSlotID)


type alias Msg =
    PopoverMsg SignupSlotID Popover.State


type alias Model =
    { signupSlotPopovers : List SignupSlotPopover }


type alias SignupSlotPopover =
    { signupSlotId : SignupSlotID
    , popoverState : Popover.State
    }


view : Html Msg
view =
    div [] [ text "signup list" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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


viewTable : Model -> Html Msg
viewTable model =
    Table.table
        { options = [ Table.attr <| class "signup-table" ]
        , thead = Table.thead [] [ viewTableColumnHeaderRow model.columns ]
        , tbody = Table.tbody [] (List.map (viewTableRow model) model.rows)
        }


initializeSignupSlotPopovers : List SignupSlot -> List SignupSlotPopover
initializeSignupSlotPopovers signupSlotList =
    List.map initializeSignupSlotPopover signupSlotList


initializeSignupSlotPopover : SignupSlot -> SignupSlotPopover
initializeSignupSlotPopover signupSlot =
    { signupSlotId = signupSlot.id, popoverState = Popover.initialState }


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
                    (viewSignupsForSlot model signupSlot model.signups)
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


popoverStateForSignupSlot : Model -> SignupSlot -> Popover.State
popoverStateForSignupSlot model signupSlot =
    List.filter (\ssp -> ssp.signupSlotId == signupSlot.id) model.signupSlotPopovers
        |> List.head
        |> Maybe.map .popoverState
        |> Maybe.withDefault Popover.initialState
