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
    { calculado : Int, operador : String, numero : Int }


inicio : Calcular
inicio =
    { calculado = 0, operador = "", numero = 0 }



-- Paso el mensaje de actualizacion


type Mensaje
    = -- Declaracion de la operaciones
      Suma
    | Resta
    | Multiplica
    | Divide
    | Reinicio
      -- Declaracion de insertado de Numeros
    | InsertarUno
    | InsertarDos
    | InsertarTres
    | InsertarCuatro
    | InsertarCinco
    | InsertarSeis
    | InsertarSiete
    | InsertarOcho
    | InsertarNueve
      -- Declaracion de insertado de Vista de Operadores
    | InsertarVSuma
    | InsertarVResta
    | InsertarVMulti
    | InsertarVDiv


actualiza : Mensaje -> Calcular -> Calcular
actualiza mensaje calcula =
    case mensaje of
        -- Operaciones
        Suma ->
            { calcula | calculado = calcula.calculado + calcula.numero }

        Resta ->
            { calcula | calculado = calcula.calculado - calcula.numero }

        Multiplica ->
            { calcula | calculado = calcula.calculado * calcula.numero }

        Divide ->
            { calcula | calculado = calcula.calculado // calcula.numero }

        Reinicio ->
            { calcula | calculado = calcula.calculado * 0 }

        -- Insertado de numeros
        InsertarUno ->
            { calcula | numero = 1 }

        InsertarDos ->
            { calcula | numero = 2 }

        InsertarTres ->
            { calcula | numero = 3 }

        InsertarCuatro ->
            { calcula | numero = 4 }

        InsertarCinco ->
            { calcula | numero = 5 }

        InsertarSeis ->
            { calcula | numero = 6 }

        InsertarSiete ->
            { calcula | numero = 7 }

        InsertarOcho ->
            { calcula | numero = 8 }

        InsertarNueve ->
            { calcula | numero = 9 }

        -- Vista de operadores
        InsertarVSuma ->
            { calcula | operador = "+" }

        InsertarVResta ->
            { calcula | operador = "-" }

        InsertarVMulti ->
            { calcula | operador = "*" }

        InsertarVDiv ->
            { calcula | operador = "/" }



-- Vista de la App


vista : Calcular -> Html Mensaje
vista resultado =
    div
        [ -- Estilos para Centrar
          style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "flex-direction" "column"
        , style "margin-top" "40px"
        ]
        [ div
            [ style "background-color" "#ecf8f8"
            , style "padding" "10px 8em"
            , style "border" "solid 1px"
            , style "border-radius" "5px"
            ]
            [ text (String.fromInt resultado.calculado) ]
        , div []
            [ btnCalc InsertarVSuma "+"
            , btnCalc InsertarVResta "-"
            , btnCalc InsertarVMulti "×"
            , btnCalc InsertarVDiv "÷"
            ]
        , div []
            [ btnCalc Reinicio "Reinicio" ]
        , div []
            [ btnCalc InsertarUno "1"
            , btnCalc InsertarDos "2"
            , btnCalc InsertarTres "3"
            , btnCalc InsertarCuatro "4"
            , btnCalc InsertarCinco "5"
            , btnCalc InsertarSeis "6"
            , btnCalc InsertarSiete "7"
            , btnCalc InsertarOcho "8"
            , btnCalc InsertarNueve "9"
            ]
        , div [] [ btnIgualdad resultado.operador ]
        ]


btnIgualdad : String -> Html Mensaje
btnIgualdad operador_actual =
    if operador_actual == "+" then
        btnCalc Suma "="

    else if operador_actual == "-" then
        btnCalc Resta "="

    else if operador_actual == "*" then
        btnCalc Multiplica "="

    else if operador_actual == "/" then
        btnCalc Divide "="

    else
        button [
        -- Estilos del Boton
        style "background-color" "#444444"
        , style "color" "white"
        , style "border" "none"
        , style "border-bottom" "solid"
        , style "border-bottom-width" "3px"
        , style "border-bottom-color" "#666666"
        , style "margin-left" "20px"
        , style "padding" "10px 4em"
        , style "border-radius" "6px"
        , style "font-weight" "700"
        , style "margin-top" "20px"]
        [ text "=" ]


btnCalc : Mensaje -> String -> Html Mensaje
btnCalc accion contenido =
    button
        [ onClick accion

        -- Estilos del Boton
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
