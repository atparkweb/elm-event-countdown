module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, h2, input, label, text, span)
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
  , started: Bool
  , valid: Bool
  }

init : Model
init =
  { name = ""
  , date = ""
  , time = ""
  , started = False
  , valid = False }

-- UPDATE
type Msg
  = NameChange String
  | DateChange String
  | TimeChange String
  | Start
  | Stop

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
      { model | started = True }
    Stop ->
      { model | started = False }


-- VIEW

type InputValidationStatus
  = Invalid String
  | Valid

view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ h1 [] [ text "Event Countdown Timer" ]
    , viewContent model
    ]

viewContent : Model -> Html Msg
viewContent model =
  if model.started then
    eventCountdown model
  else
    eventForm model

eventForm : Model -> Html Msg
eventForm model =
  div [ class "event-form inner-content"]
        [ viewInput "name-input" "Event" "text" "Event Name" model.name True NameChange validateRequired
          , viewInput "date-input" "Date" "text" "yyyy/mm/dd" model.date True DateChange validateDate
          , viewInput "time-input" "Time (optional)" "text" "hh:mm AM/PM" model.time False TimeChange validateTime
          , button [ class "button", onClick Start, disabled model.started ] [ text "Start" ]
        ]

eventCountdown : Model -> Html Msg
eventCountdown model =
  div [ class "event-countdown inner-content" ]
    [ h2 [] [ text (countdownTimer model) ]
    , div [] [ text ("until " ++ model.name) ]
    , button [ class "button", onClick Stop ] [ text "Clear" ]
    ]

countdownTimer : Model -> String
countdownTimer model =
  model.date

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
    Invalid "This field is required"
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
    Invalid "Date is not in the correct format"

validateTime : String -> InputValidationStatus
validateTime val =
  if Regex.contains timePattern val then
    Valid
  else
    Invalid "Time is not in the correct format"

getValidationClass : Bool -> String
getValidationClass x =
  if x then
    "valid"
  else
    "invalid"

validationMessage : InputValidationStatus -> Html Msg
validationMessage result =
  case result of
    Invalid msg ->
      spanWithClass (getValidationClass False) msg
    Valid ->
      spanWithClass (getValidationClass True) "Ok" 

spanWithClass : String -> String -> Html Msg
spanWithClass className content =
  span [ class "field-info", class className ] [ text content ]