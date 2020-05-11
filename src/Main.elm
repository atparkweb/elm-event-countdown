module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, input, label, text, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Regex
import String exposing (isEmpty, trim)

-- MAIN
main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL
type alias Model =
  { name: String
  , date: String
  , time: String
  }

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

type InputValidationStatus
  = Required String
  | InvalidFormat String
  | Valid

view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ h1 [] [ text "Event Countdown Timer" ]
    , div [ class "event-form"]
      [ viewInput "name-input" "Event" "text" "Event Name" model.name True NameChange validateRequired
        , viewInput "date-input" "Date" "text" "yyyy/mm/dd" model.date True DateChange validateDate
        , viewInput "time-input" "Time" "text" "hh:mm AM/PM" model.time False TimeChange validateTime
        , button [ class "button", onClick Start ] [ text "Start" ]
    ]
  ]

viewInput : String -> String -> String -> String -> String -> Bool -> (String -> Msg) -> (String -> InputValidationStatus) -> Html Msg
viewInput i l t p v r toMsg validationResult =
  div [ class "form-control" ]
    [ label [ for i ] [ text l ]
    , input [ id i, type_ t, placeholder p, value v, required r, onInput toMsg ] []
    , validationMessage (validationResult v)
    ]

validateRequired : String -> InputValidationStatus
validateRequired val =
  if val |> trim |> isEmpty then
    Required "This field is required"
  else
    Valid

datePattern : Regex.Regex
datePattern =
  Maybe.withDefault Regex.never <|
    Regex.fromString "\\d\\d\\d\\d(/|-)\\d\\d(/|-)\\d\\d"

timePattern : Regex.Regex
timePattern =
  Maybe.withDefault Regex.never <|
    Regex.fromStringWith { caseInsensitive = True, multiline = False } "(\\d\\d:\\d\\d ?((A|P)M)|^(?!.))"

validateDate : String -> InputValidationStatus
validateDate val =
  if Regex.contains datePattern val then
    Valid
  else
    InvalidFormat "Date is not in the correct format"

validateTime : String -> InputValidationStatus
validateTime val =
  if Regex.contains timePattern val then
    Valid
  else
    InvalidFormat "Time is not in the correct format"

boolToString : Bool -> String
boolToString i =
  if i then
    "OK"
  else
    "Invalid"

getValidationClass : Bool -> String
getValidationClass x =
  if x then
    "valid"
  else
    "invalid"

validationMessage : InputValidationStatus -> Html msg
validationMessage result =
  case result of
    Required msg ->
      span [ class "field-info invalid" ] [ text msg ]
    InvalidFormat msg ->
      span [ class "field-info invalid" ] [ text msg ]
    Valid ->
      span [ class "field-info valid" ] [ text "Ok" ]