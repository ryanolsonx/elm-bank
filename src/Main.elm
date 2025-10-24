module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias ConfigModel =
    { playerNames : List String }


type Page
    = ConfigPage ConfigModel
    | GamePage


type alias Model =
    Page


init : Model
init =
    ConfigPage { playerNames = [ "", "" ] }



-- UPDATE


type Msg
    = StartGame


update : Msg -> Model -> Model
update msg _ =
    case msg of
        StartGame ->
            GamePage



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        ConfigPage _ ->
            div [] [ text "Config", button [ onClick StartGame ] [ text "Start Game" ] ]

        GamePage ->
            div [] [ text "Game" ]
