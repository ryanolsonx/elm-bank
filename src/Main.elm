module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, h1, input, li, text, ul)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias ConfigModel =
    { playerNames : List String
    , totalRounds : Int
    }


type alias Player =
    { name : String }


type alias Round =
    { turns : Int }


type alias GameModel =
    { previousRounds : List Round
    , currentRound : Maybe Round
    , nextRounds : List Round
    , players : List Player
    }


type Page
    = ConfigPage ConfigModel
    | GamePage GameModel


type alias Model =
    Page


init : Model
init =
    ConfigPage { playerNames = [ "", "" ], totalRounds = 15 }


initRound : Round
initRound =
    { turns = 0 }


initPlayer : String -> Player
initPlayer name =
    { name = name }



-- UPDATE


type Msg
    = StartGame
    | AddPlayer
    | PlayerNameChange Int String


updateElement : List a -> Int -> a -> List a
updateElement list index to =
    let
        updateElementInternal i curr =
            if i == index then
                to

            else
                curr
    in
    List.indexedMap updateElementInternal list


update : Msg -> Model -> Model
update msg model =
    case msg of
        -- Config Page
        StartGame ->
            case model of
                ConfigPage configModel ->
                    let
                        nextRounds =
                            List.map (\_ -> initRound) (List.range 0 (configModel.totalRounds - 1))

                        players =
                            List.map initPlayer configModel.playerNames
                    in
                    GamePage
                        { previousRounds = []
                        , currentRound = Just initRound
                        , nextRounds = nextRounds
                        , players = players
                        }

                GamePage _ ->
                    model

        PlayerNameChange index newName ->
            case model of
                ConfigPage configModel ->
                    let
                        nextPlayerNames =
                            updateElement configModel.playerNames index newName

                        nextModel =
                            { configModel | playerNames = nextPlayerNames }
                    in
                    ConfigPage nextModel

                GamePage _ ->
                    model

        AddPlayer ->
            case model of
                ConfigPage configModel ->
                    let
                        nextModel =
                            -- Add new player to the end
                            { configModel | playerNames = List.append configModel.playerNames [ "" ] }
                    in
                    ConfigPage nextModel

                GamePage _ ->
                    model



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        ConfigPage configModel ->
            viewConfigPage configModel

        GamePage gameModel ->
            viewGamePage gameModel


viewConfigPage : ConfigModel -> Html Msg
viewConfigPage model =
    div []
        [ h1 [] [ text "Config" ]
        , viewPlayers model.playerNames
        , button [ onClick AddPlayer ] [ text "+ Add Another" ]
        , button [ onClick StartGame ] [ text "Start Game" ]
        ]


viewPlayers : List String -> Html Msg
viewPlayers playerNames =
    ul []
        (List.indexedMap
            (\i name -> li [] [ viewPlayerTextBox i name ])
            playerNames
        )


viewPlayerTextBox : Int -> String -> Html Msg
viewPlayerTextBox playerIndex playerName =
    input [ value playerName, onInput (PlayerNameChange playerIndex) ] []



-- GAME


viewGamePage : GameModel -> Html Msg
viewGamePage model =
    div []
        [ h1 [] [ text "Game" ]
        , viewPlayers (List.map (\player -> player.name) model.players)
        ]
