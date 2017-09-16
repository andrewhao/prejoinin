module Request.Sheet exposing (..)

import Data.Sheet exposing (Signup, Sheet, SheetID, decodeRows, decodeColumns, decodeSignups, decodeSignupSlots, Name, Comment, Email, SignupSlotID)
import Json.Decode exposing (Decoder, bool, field, map4, map8, string)
import Json.Encode
import HttpBuilder
import Http


getSheetDetails : String -> SheetID -> String -> Http.Request Sheet
getSheetDetails apiBaseEndpoint sheetId apiKey =
    let
        url =
            apiBaseEndpoint ++ "/api/v1/sheets/" ++ sheetId ++ ".json"
    in
        HttpBuilder.get url
            |> HttpBuilder.withHeaders [ ( "Content-Type", "application/json" ), ( "Authorization", "WejoininAuth key=" ++ apiKey ) ]
            |> HttpBuilder.withExpect (Http.expectJson decodeSheetResponse)
            |> HttpBuilder.toRequest



-- |> HttpBuilder.send ReceiveSheetDetails


decodeSignupResponse : Decoder Signup
decodeSignupResponse =
    map4 Signup
        (field "id" string)
        (field "signup_slot_id" string)
        (field "name" string)
        (field "comment" string)


decodeSheetResponse : Decoder Sheet
decodeSheetResponse =
    map8 Sheet
        (field "id" string)
        (field "title" string)
        (field "description" string)
        (field "rows" decodeRows)
        (field "columns" decodeColumns)
        (field "signup_slots" decodeSignupSlots)
        (field "signups" decodeSignups)
        (field "is_name_visible" bool)



-- Should this be a Data thing?


encodedSignupValue : Maybe SignupSlotID -> Maybe Name -> Maybe Email -> Maybe Comment -> Json.Encode.Value
encodedSignupValue focusedSlotId name email comment =
    Json.Encode.object
        [ ( "signup_slot_id", Json.Encode.string (Maybe.withDefault "" focusedSlotId) )
        , ( "name", Json.Encode.string (Maybe.withDefault "" name) )
        , ( "email", Json.Encode.string (Maybe.withDefault "" email) )
        , ( "comment", Json.Encode.string (Maybe.withDefault "" comment) )
        ]


createSignup : String -> String -> Maybe SignupSlotID -> Maybe Name -> Maybe Email -> Maybe Comment -> Http.Request Signup
createSignup apiBaseEndpoint apiKey focusedSlotId name email comment =
    let
        url =
            apiBaseEndpoint ++ "/api/v1/signups"
    in
        HttpBuilder.post url
            |> HttpBuilder.withHeaders [ ( "Content-Type", "application/json" ), ( "Authorization", "WejoininAuth key=" ++ apiKey ) ]
            |> HttpBuilder.withJsonBody (encodedSignupValue focusedSlotId name email comment)
            |> HttpBuilder.withExpect (Http.expectJson decodeSignupResponse)
            |> HttpBuilder.toRequest



-- |> HttpBuilder.send ReceiveSignupResponse
