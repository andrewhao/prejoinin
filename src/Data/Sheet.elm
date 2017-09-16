{- Sheet data structure, plus the JSON encoding/decoding mappings that accompany
   it. This organization structure is modeled after @rtfeldman's
   https://github.com/rtfeldman/elm-spa-example.
-}


module Data.Sheet exposing (..)

import Json.Decode exposing (Decoder, bool, field, int, map3, map4, map5, string)


-- MODEL


type alias Sheet =
    { id : String
    , title : String
    , description : String
    , rows : List Row
    , columns : List Column
    , signupSlots : List SignupSlot
    , signups : List Signup
    , isNameVisible : Bool
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


type alias SheetID =
    String


type alias RowID =
    String


type alias SignupSlotID =
    String


type alias SignupID =
    String


type alias Name =
    String


type alias Email =
    String


type alias Comment =
    String



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
