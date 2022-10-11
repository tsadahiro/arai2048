module Orientation exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes
import Html.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Json.Decode as Decode


main = Browser.element {init = init
                       ,update = update
                       ,view = view
                       ,subscriptions = subscriptions
                       }

type alias Model = {}
type Msg = OrientationChanged

init : () -> (Model, Cmd Msg)
init _ = ({}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        OrientationChanged ->
            let
                dummy = Debug.log "orientation" "changed"
            in
                ({}, Cmd.none)

view : Model -> Html Msg
view model =
    Html.div [onOrientationChanged OrientationChanged ][]

onOrientationChanged : Msg -> Html.Attribute Msg
onOrientationChanged message =
  on "click" (Decode.succeed message)        

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
        
