import Html exposing (Html, button, div, text, Attribute)
import Html.Attributes exposing (style, id)
import Html.App as Html
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Debug exposing (log)

main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL
type alias Model =
  { x : Int
  , y : Int }

model : Model
model = Model 0 0

-- UPDATE

type Msg = MoveUp | MoveDown | MoveLeft | MoveRight

update : Msg -> Model -> Model
update msg model =
  case msg of
    MoveUp ->
      Model model.x (model.y + 1)
    MoveDown ->
      Model model.x (model.y - 1)
    MoveLeft ->
      Model (model.x - 1) model.y
    MoveRight ->
      Model (model.x + 1) model.y

-- VIEW

myStyle : Model -> Attribute msg
myStyle model =
  style
  [ ("position", "absolute")
  , ("color", "red")
  , ("left", (toString model.x ++ "px"))
  , ("top", (toString model.y ++ "px"))
  ]

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick MoveDown ] [ text "-" ]
    , button [ onClick MoveLeft ] [ text "<-" ]
    , div [] [ text (toString model) ]
    , button [ onClick MoveUp ] [ text "+" ]
    , button [ onClick MoveRight ] [ text "->" ]
    , div [ (id "container"), (style [ ("position", "relative") ] ) ]
      [ div [ myStyle model ]
        [ text "hello" ] ]
    ]
