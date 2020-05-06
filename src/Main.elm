module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

-- MAIN
main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL
type alias Model =
  { name: String
    , date: String 
  }

init : Model
init =
  { name = "",
    date = "" }

-- UPDATE
type Msg
  = NameChange String
  | DateChange String

update : Msg -> Model -> Model
update msg model =
  case msg of
    NameChange newName ->
      { model | name = newName }
    DateChange newDate ->
      { model | date = newDate }


-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ label [ for "name-input" ] [ text "Event" ]
    , input [ id "name-input", placeholder "Event Name", value model.name, onInput NameChange, class "form-control" ] []
    , label [ for "date-input" ] [ text "Date" ]
    , input [ id "date-input", type_ "date", value model.date, onInput DateChange, class "form-control" ] []
    , button [] [ text "Submit" ]
    ]