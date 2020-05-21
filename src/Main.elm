module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, h2, input, label, text, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Iso8601
import Regex
import String exposing (isEmpty, trim)
import Task
import Time exposing (Month(..))

-- MAIN
main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


-- MODEL
type alias Model =
  { event: Event
  , nameInput: String
  , dateInput: String
  , timeInput: String
  , started: Bool
  , valid: Bool
  , time: Time.Posix
  }

type alias Event =
  { name: String
  , time: Time.Posix
  , timezone: Time.Zone
  }

init : () -> (Model, Cmd Msg)
init _ =
  ({ event =
     { name = ""
     , time = Time.millisToPosix 0
     , timezone = Time.utc
     }
   , nameInput = ""
   , dateInput = ""
   , timeInput = ""
   , started = False
   , valid = False
   , time = Time.millisToPosix 0
   }
  , Task.perform SetTimezone Time.here)

-- UPDATE
type Msg
  = NameChange String
  | DateChange String
  | TimeChange String
  | SetTimezone Time.Zone
  | Start
  | Stop
  | Tick Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NameChange newName ->
      ({ model | nameInput = newName }
      , Cmd.none)
    DateChange newDate ->
      ({ model | dateInput = newDate}
      , Cmd.none)
    TimeChange newTime ->
      if String.isEmpty newTime then
        ({ model | timeInput = "00:00"}, Cmd.none)
      else
        ({ model | timeInput = newTime}, Cmd.none)
    SetTimezone newZone ->
      ({ model | event = updateEventZone newZone model.event }
      , Cmd.none)
    Start ->
      ({ model | started = True }
      , Cmd.none)
    Stop ->
      ({ model | started = False }
      , Cmd.none)
    Tick newTime ->
      ({ model | time = (calculateRemainingTime newTime model) }
      , Cmd.none)

updateEventName : String -> Event -> Event
updateEventName newName oldEvent =
  { oldEvent | name = newName }

updateEventTime : Time.Posix -> Event -> Event
updateEventTime newTime oldEvent =
  { oldEvent | time = newTime}

updateEventZone : Time.Zone -> Event -> Event
updateEventZone newZone oldEvent =
  { oldEvent | timezone = newZone }

calculateRemainingTime : Time.Posix -> Model -> Time.Posix
calculateRemainingTime currentTime model =
  -- Caculate time left to event
  currentTime


-- SUBSCRIPTIONSS
subscriptions : Model -> Sub Msg
subscriptions model =
  if model.started then
    Time.every 1000 Tick
  else
    Sub.none


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
    viewCountdown model
  else
    viewForm model

viewForm : Model -> Html Msg
viewForm model =
  div [ class "event-form inner-content"]
        [ viewInput "name-input" "Event" "text" "Event Name" model.event.name True NameChange validateRequired
          , viewInput "date-input" "Date" "date" "yyyy/mm/dd" (dateString model.event.timezone model.event.time) True DateChange validateDate
          , viewInput "time-input" "Time (optional)" "time" "" (timeString model.event.timezone model.event.time) False TimeChange validateTime
          , button [ class "button", onClick Start, disabled model.started ] [ text "Start" ]
        ]

dateString : Time.Zone -> Time.Posix -> String
dateString zone date =
  [ Time.toYear zone date
  , Time.toMonth zone date |> monthAsInt
  , Time.toDay zone date
  ]
  |> List.map String.fromInt
  |> String.join "/"

monthAsInt : Month -> Int
monthAsInt month =
  case month of
    Jan -> 1
    Feb -> 2
    Mar -> 3
    Apr -> 4
    May -> 5
    Jun -> 6
    Jul -> 7
    Aug -> 8
    Sep -> 9
    Oct -> 10
    Nov -> 11
    Dec -> 12
  
timeString : Time.Zone -> Time.Posix -> String
timeString zone time =
  [ Time.toHour zone time
    , Time.toMinute zone time
    , Time.toSecond zone time
  ]
  |> List.map String.fromInt
  |> String.join ":"

viewCountdown : Model -> Html Msg
viewCountdown model =
  div [ class "event-countdown inner-content" ]
    [ h2 [] [ text (countdownTimer model) ]
    , div [] [ text ("until " ++ model.event.name) ]
    , button [ class "button", onClick Stop ] [ text "Clear" ]
    ]

countdownTimer : Model -> String
countdownTimer model =
  "00:00:00:00"

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
    Regex.fromString "\\d\\d\\d\\d-\\d\\d-\\d\\d"

timePattern : Regex.Regex
timePattern =
  Maybe.withDefault Regex.never <|
    Regex.fromStringWith { caseInsensitive = True, multiline = False } "(\\d\\d:\\d\\d ?|^(?!.))"

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
    Invalid "Time is not in the correct format (e.g. hh:mm) "

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
