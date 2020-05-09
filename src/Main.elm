module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, input, text, label, form, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

-- MAIN
main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL
type alias Model =
  { name: String
  , date: String
  , time: String
  }

type alias IsRequired = Bool

init : Model
init =
  { name = ""
  , date = ""
  , time = ""}

-- UPDATE
type Msg
  = NameChange String
  | DateChange String
  | TimeChange String
  | Start

update : Msg -> Model -> Model
update msg model =
  case msg of
    NameChange newName ->
      { model | name = newName }
    DateChange newDate ->
      { model | date = newDate }
    TimeChange newTime ->
      { model | time = newTime }
    Start ->
      model


-- VIEW
view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ h1 [] [ text "Event Countdown Timer" ]
    , div [ class "event-form"]
      [ viewInput "name-input" "Event" "text" "Event Name" model.name True NameChange
      , viewInput "date-input" "Date" "text" "Event Date" model.date True DateChange
      , viewInput "time-input" "Time" "text" "Event Time" model.time False TimeChange
      , button [ class "button", onClick Start ] [ text "Start" ]
    ]
  ]

viewInput : String -> String -> String -> String -> String -> IsRequired -> (String -> msg) -> Html msg
viewInput i labelText t p v r toMsg =
  div [ class "form-control" ]
    [ label [ for i ] [ text labelText ]
    , input [ id i, type_ t, placeholder p, value v, required r, onInput toMsg ] []
    , span [ class "field-info" ] [ text "validation here"]
    ]
