module Calculadora exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



-- Declaro el principal que puede ser Sandbox, element u otros...
-- En este caso "sandbox"  por que no quiero interoperabilidad con JS


main =
    Browser.sandbox { init = inicio, update = actualiza, view = vista }



-- Inicio la variable principal, que se pasara como Mensaje


type alias Calcular =
    { calculado : Int, operador : String, numero : Int }


inicio : Calcular
inicio =
    { calculado = 0, operador = "", numero = 0 }



-- Determina el valor y donde es insertado


type alias Numero =
    { valor : Int, yaInserto : Bool }



-- Paso el mensaje de actualizacion


type Mensaje
    = -- Declaracion de la operaciones
      Suma
    | Resta
    | Multiplica
    | Divide
    | Reinicio
      -- Declaracion para insertar numeros
    | Insertar Numero
      -- Declaracion de insertado de Vista de Operadores
    | Operador String


actualiza : Mensaje -> Calcular -> Calcular
actualiza mensaje calcula =
    case mensaje of
        -- Operaciones
        -- Todos los operadores hacen la operacion y borrar el segundo numero del calculo y el operador utilizado
        Suma ->
            { calcula | calculado = calcula.calculado + calcula.numero, operador = "", numero = 0 }

        Resta ->
            { calcula | calculado = calcula.calculado - calcula.numero, operador = "", numero = 0 }

        Multiplica ->
            { calcula | calculado = calcula.calculado * calcula.numero, operador = "", numero = 0 }

        Divide ->
            { calcula | calculado = calcula.calculado // calcula.numero, operador = "", numero = 0 }

        Reinicio ->
            { calcula | calculado = calcula.calculado * 0, numero = calcula.numero * 0, operador = "" }

        -- Insertado de numeros
        -- De todos los valores insertados, se evaluan si son iguales a cero
        -- Sino es el caso transforma el valor anterior en cadena y luego en numero nuevamente
        -- para las operaciones
        Insertar num ->
            if num.yaInserto then
                if calcula.numero == 0 then
                    { calcula | numero = num.valor }

                else
                    { calcula | numero = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.numero ++ String.fromInt num.valor)) }

            else if calcula.calculado == 0 then
                { calcula | calculado = num.valor }

            else
                { calcula | calculado = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.calculado ++ String.fromInt num.valor)) }

        -- Vista de operadores
        -- Agrega el operador con el que determina el calculo
        Operador signo ->
            { calcula | operador = signo }



-- Vista de la App (vista final)


vista : Calcular -> Html Mensaje
vista resultado =
    div
        [ -- Estilos para Centrar toda la pagina
          style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        ]
        [ div
            [ -- Estilos para la calculadora
              style "display" "flex"
            , style "align-items" "center"
            , style "flex-direction" "column"
            , style "margin-top" "40px"
            , style "background-color" "gray"
            , style "border-color" "black"
            , style "border" "none"
            , style "border-bottom" "solid"
            , style "border-bottom-width" "3px"
            , style "border-bottom-color" "#666666"
            , style "border-right" "solid"
            , style "border-right-color" "#666666"
            , style "padding-top" "20px"
            , style "padding-bottom" "20px"
            , style "width" "500px"
            , style "border-radius" "8px"
            ]
            -- Estilos para el panel
            [ div
                [ style "background-color" "#ecf8f8"
                , style "padding" "10px 4px"
                , style "border-radius" "5px"
                , style "border" "none"
                , style "border-bottom" "solid"
                , style "border-bottom-width" "3px"
                , style "border-bottom-color" "rgba(0,0,0,0.6)"
                , style "border-right" "solid"
                , style "border-right-color" "rgba(0,0,0,0.6)"
                , style "font-size" "xxx-large"
                , style "overflow-x" "auto"
                , style "width" "400px"
                , style "text-align" "center"
                ]
                -- Contenido del Panel
                [ text (panelDeCalculo resultado.calculado resultado.operador resultado.numero) ]

            -- botones de la calculadora
            , div [ style "display" "flex", style "flex-direction" "row-reverse" ]
                [ div [ style "display" "flex", style "flex-direction" "column", style "padding-right" "20px", style "padding-top" "10px" ]
                    [ btnOperador (Operador "+") "+"
                    , btnOperador (Operador "-") "-"
                    , btnOperador (Operador "*") "×"
                    , btnOperador (Operador "/") "÷"
                    ]
                , insercionNumeros resultado.operador
                ]
            , div [] [ btnCalc Reinicio "C", insercionCero resultado.operador, btnIgualdad resultado.operador ]
            ]
        ]



-- Componentes de la vista
-- Evalua si el operador esta declarado para insertar el segundo numero
-- de la operacion matematica


insercionNumeros : String -> Html Mensaje
insercionNumeros operador =
    if String.isEmpty operador then
        -- Botones para el primer numero
        div [ style "display" "flex", style "flex-direction" "column" ]
            [ div [ style "display" "flex", style "flex-direction" "row" ]
                [ btnCalc (Insertar (Numero 1 False)) "1"
                , btnCalc (Insertar (Numero 2 False)) "2"
                , btnCalc (Insertar (Numero 3 False)) "3"
                ]
            , div [ style "display" "flex", style "flex-direction" "row" ]
                [ btnCalc (Insertar (Numero 4 False)) "4"
                , btnCalc (Insertar (Numero 5 False)) "5"
                , btnCalc (Insertar (Numero 6 False)) "6"
                ]
            , div [ style "display" "flex", style "flex-direction" "row" ]
                [ btnCalc (Insertar (Numero 7 False)) "7"
                , btnCalc (Insertar (Numero 8 False)) "8"
                , btnCalc (Insertar (Numero 9 False)) "9"
                ]
            ]

    else
        -- Botones para el segundo numero
        div [ style "display" "flex", style "flex-direction" "column" ]
            [ div [ style "display" "flex", style "flex-direction" "row" ]
                [ btnCalc (Insertar (Numero 1 True)) "1"
                , btnCalc (Insertar (Numero 2 True)) "2"
                , btnCalc (Insertar (Numero 3 True)) "3"
                ]
            , div [ style "display" "flex", style "flex-direction" "row" ]
                [ btnCalc (Insertar (Numero 4 True)) "4"
                , btnCalc (Insertar (Numero 5 True)) "5"
                , btnCalc (Insertar (Numero 6 True)) "6"
                ]
            , div [ style "display" "flex", style "flex-direction" "row" ]
                [ btnCalc (Insertar (Numero 7 True)) "7"
                , btnCalc (Insertar (Numero 8 True)) "8"
                , btnCalc (Insertar (Numero 9 True)) "9"
                ]
            ]



-- Lo mismo que a los demas numeros para solo para el "Cero"


insercionCero : String -> Html Mensaje
insercionCero operador =
    if String.isEmpty operador then
        -- Boton Cero para el primer numero
        btnCalc (Insertar (Numero 0 True)) "0"

    else
        -- Boton Cero para el segundo numero
        btnCalc (Insertar (Numero 0 False)) "0"



-- Muestra los resultados y operadores insertados
-- Ademas evalua si el segundo numero deberia ser visible


panelDeCalculo : Int -> String -> Int -> String
panelDeCalculo primer_numero operador segundo_numero =
    if segundo_numero == 0 then
        String.fromInt primer_numero ++ operador

    else
        String.fromInt primer_numero ++ operador ++ String.fromInt segundo_numero



-- Estilo de Boton
-- para el boton igual sin parametros


btnIgualDesactivado : Html Mensaje
btnIgualDesactivado =
    button
        [ -- Estilos del Boton
          style "background-color" "#444444"
        , style "color" "white"
        , style "border" "none"
        , style "border-bottom" "solid"
        , style "border-bottom-width" "3px"
        , style "border-bottom-color" "#666666"
        , style "margin-left" "20px"
        , style "margin-right" "20px"
        , style "padding" "20px 100px"
        , style "border-radius" "6px"
        , style "font-weight" "700"
        , style "font-size" "xx-large"
        , style "margin-top" "20px"
        ]
        [ text "=" ]


btnIgual : Mensaje -> Html Mensaje
btnIgual accion =
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
        , style "margin-right" "20px"
        , style "padding" "20px 100px"
        , style "border-radius" "6px"
        , style "font-weight" "700"
        , style "font-size" "xx-large"
        , style "margin-top" "20px"
        ]
        [ text "=" ]



-- El boton de igualdad determina el operador y la funcion a utilizar


btnIgualdad : String -> Html Mensaje
btnIgualdad operador_actual =
    if operador_actual == "+" then
        btnIgual Suma

    else if operador_actual == "-" then
        btnIgual Resta

    else if operador_actual == "*" then
        btnIgual Multiplica

    else if operador_actual == "/" then
        btnIgual Divide

    else
        btnIgualDesactivado



-- Boton estilizado para los operadores


btnOperador : Mensaje -> String -> Html Mensaje
btnOperador accion contenido =
    button
        [ onClick accion

        -- Estilos del Boton
        , style "background-color" "#733d3d"
        , style "color" "white"
        , style "border" "none"
        , style "border-bottom" "solid"
        , style "border-bottom-width" "3px"
        , style "border-bottom-color" "#666666"
        , style "margin-left" "20px"
        , style "padding" "10px 2em"
        , style "border-radius" "6px"
        , style "font-weight" "700"
        , style "font-size" "x-large"
        , style "margin-top" "20px"
        ]
        [ text contenido ]



-- Boton estilizado para accion cualquier accion dentro de la calculadora


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
        , style "padding" "20px 1em"
        , style "border-radius" "6px"
        , style "font-weight" "700"
        , style "font-size" "32px"
        , style "margin-top" "20px"
        ]
        [ text contenido ]
