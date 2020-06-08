module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, h2, input, label, text, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Regex
import String exposing (isEmpty, trim)
import Task
import Time exposing (Month(..))
import Utc

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
  , time: Int
  }

type alias Event =
  { name: String
  , time: Int
  , timezone: Time.Zone
  }

init : () -> (Model, Cmd Msg)
init _ =
  ({ event =
     { name = ""
     , time = 0
     , timezone = Time.utc
     }
   , nameInput = ""
   , dateInput = ""
   , timeInput = ""
   , started = False
   , valid = False
   , time = 0
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
      (startEvent model
      , Cmd.none)
    Stop ->
      ({ model | started = False }
      , Cmd.none)
    Tick newTime ->
      ({ model | time = calculateRemainingTime (Time.posixToMillis newTime) model.event.time }
      , Cmd.none)

eventInputToTime : Model -> Int
eventInputToTime model =
    case Utc.toTime (utcString model.dateInput model.timeInput) of
      Ok time ->
        Time.posixToMillis time
      Err _ ->
        0

utcString : String -> String -> String
utcString date time =
  String.join "" [date, time]

startEvent : Model -> Model
startEvent model =
  { model | event = updateEventName model.nameInput model.event, started = True }

saveEventTime : Model -> Model
saveEventTime model =
  let eventTime = eventInputToTime model in
    { model | event = updateEventTime eventTime model.event }


updateEventName : String -> Event -> Event
updateEventName newName oldEvent =
  { oldEvent | name = newName }

updateEventTime : Int -> Event -> Event
updateEventTime newTime oldEvent =
  { oldEvent | time = newTime}

updateEventZone : Time.Zone -> Event -> Event
updateEventZone newZone oldEvent =
  { oldEvent | timezone = newZone }

calculateRemainingTime : Int -> Int -> Int
calculateRemainingTime currentTime eventTime =
  eventTime - currentTime

-- SUBSCRIPTIONSS
subscriptions : Model -> Sub Msg
subscriptions model =
  if model.started then
    Time.every 1000 Tick
  else
    Sub.none


-- VIEW
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
        [ viewInput "name-input" "Event" "text" "Event Name" model.nameInput True NameChange validateRequired
          , viewInput "date-input" "Date" "date" "yyyy/mm/dd" model.dateInput True DateChange validateDate
          , viewInput "time-input" "Time (optional)" "time" "" model.timeInput False TimeChange validateTime
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
    [ h2 [] [ text (countdownTimer model.time) ]
    , div [] [ text ("until " ++ model.event.name) ]
    , button [ class "button", onClick Stop ] [ text "Clear" ]
    ]

countdownTimer : Int -> String
countdownTimer time =
  [millisToHours time, millisToMinutes time, millisToSeconds time]
  |> List.map String.fromInt
  |> String.join ":"

millisToSeconds : Int -> Int
millisToSeconds ms =
  let m = toFloat ms in
    modBy 60 (floor (m / 1000))

millisToMinutes : Int -> Int
millisToMinutes ms =
  let m = toFloat ms in
    modBy 60 (floor (m / (1000 * 60)))

millisToHours : Int -> Int
millisToHours ms =
  let m = toFloat ms in
    modBy 24 (floor (m / (1000 * 60 * 60)))

type InputValidationStatus
  = Invalid String
  | Valid

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
