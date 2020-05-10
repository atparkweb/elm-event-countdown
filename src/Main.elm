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

type InputStatus
  = Required String
  | Invalid String
  | Valid String

view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ h1 [] [ text "Event Countdown Timer" ]
    , div [ class "event-form"]
      [ viewInput "name-input" "Event" "text" "Event Name" model.name True NameChange (validateRequired model.name)
        , viewInput "date-input" "Date" "text" "yyyy/mm/dd" model.date True DateChange (validateDate model.date)
        , viewInput "time-input" "Time" "text" "hh:mm AM/PM" model.time False TimeChange (validateTime model.time)
        , button [ class "button", onClick Start ] [ text "Start" ]
    ]
  ]

viewInput : String -> String -> String -> String -> String -> Bool -> (String -> Msg) -> Bool -> Html Msg
viewInput i l t p v r toMsg isValid =
  div [ class "form-control" ]
    [ label [ for i ] [ text l ]
    , input [ id i, type_ t, placeholder p, value v, required r, onInput toMsg ] []
    , validationMessage isValid
    ]

validateRequired : String -> Bool
validateRequired val =
  val
  |> trim
  |> isEmpty
  |> not

datePattern : Regex.Regex
datePattern =
  Maybe.withDefault Regex.never <|
    Regex.fromString "\\d\\d\\d\\d(/|-)\\d\\d(/|-)\\d\\d"

timePattern : Regex.Regex
timePattern =
  Maybe.withDefault Regex.never <|
    Regex.fromStringWith { caseInsensitive = True, multiline = False } "(\\d\\d:\\d\\d ?((A|P)M)|^(?!.))"
  

validateDate : String -> Bool
validateDate val =
  Regex.contains datePattern val

validateTime : String -> Bool
validateTime val =
  Regex.contains timePattern val

boolToString : Bool -> String
boolToString i =
  if i then
    "OK"
  else
    "Not OK"

getValidationClass : Bool -> String
getValidationClass x =
  if x then
    "valid"
  else
    "invalid"

validationMessage : Bool -> Html msg
validationMessage isValid =
  span [class "field-info", class (isValid |> getValidationClass) ] [ text (isValid |> boolToString) ]