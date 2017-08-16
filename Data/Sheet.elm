{- Sheet data structure, plus the JSON encoding/decoding mappings that accompany
   it. This organization structure is modeled after @rtfeldman's
   https://github.com/rtfeldman/elm-spa-example.
-}


module Data.Sheet exposing (..)

import Json.Decode exposing (Decoder, map3, field, string, int, map5, map4, bool)


-- MODEL


type alias SheetJSONResponse =
    { title : String
    , description : String
    , rows : List Row
    , columns : List Column
    , signupSlots : List SignupSlot
    , signups : List Signup
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


type alias Signup =
    { id : String
    , signupSlotId : String
    , name : String
    , comment : String
    }



-- JSON


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


decodeSignups : Decoder (List Signup)
decodeSignups =
    Json.Decode.list decodeSignup


decodeSignup : Decoder Signup
decodeSignup =
    map4 Signup
        (field "id" string)
        (field "signup_slot_id" string)
        (field "name" string)
        (field "comment" string)
