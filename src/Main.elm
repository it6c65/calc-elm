module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



-- Declaro el principal que puede ser Sandbox, element u otros...


main =
    Browser.sandbox { init = inicio, update = actualiza, view = vista }



-- Inicio la variable principal


type alias Calcular =
    Int


inicio : Calcular
inicio =
    0



-- Paso el mensaje de actualizacion


type Mensaje
    = Suma
    | Resta
    | Multiplica
    | Divide


actualiza : Mensaje -> Calcular -> Calcular
actualiza mensaje calcula =
    case mensaje of
        Suma ->
            calcula + 1

        Resta ->
            calcula - 1

        Multiplica ->
            calcula * 2

        Divide ->
            calcula // 2


-- Vista de la App


vista : Calcular -> Html Mensaje
vista resultado =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "flex-direction" "column"
        , style "margin-top" "40px"
        ]
        [ div [ style "background-color" "#ecf8f8"
              , style "padding" "10px 8em"
              ] [ text (String.fromInt resultado) ]
        , div []
            [ btnCalc Suma "+"
            , btnCalc Resta "-"
            , btnCalc Multiplica "×"
            , btnCalc Divide "÷"
            ]
        ]


btnCalc : Mensaje -> String -> Html Mensaje
btnCalc accion contenido =
    button
        [ onClick accion
        , style "background-color" "#444444"
        , style "color" "white"
        , style "border" "none"
        , style "border-bottom" "solid"
        , style "border-bottom-width" "3px"
        , style "border-bottom-color" "#666666"
        , style "margin-left" "20px"
        , style "padding" "10px 4em"
        , style "border-radius" "6px"
        , style "font-weight" "700"
        , style "margin-top" "20px"
        ]
        [ text contenido ]
