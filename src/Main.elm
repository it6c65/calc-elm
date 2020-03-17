module Main exposing (..)

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



-- Paso el mensaje de actualizacion


type Mensaje
    = -- Declaracion de la operaciones
      Suma
    | Resta
    | Multiplica
    | Divide
    | Reinicio
      -- Declaracion para insertar el primer numero
    | InsertarUnoAlprincipio
    | InsertarDosAlprincipio
    | InsertarTresAlprincipio
    | InsertarCuatroAlprincipio
    | InsertarCincoAlprincipio
    | InsertarSeisAlprincipio
    | InsertarSieteAlprincipio
    | InsertarOchoAlprincipio
    | InsertarNueveAlprincipio
      -- Declaracion de insertado de Numeros (el sengundo)
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

        -- Insertado de numeros del primer numero
        -- De todos los valores insertados, se evaluan si son iguales a cero
        -- Sino es el caso transforma el valor anterior en cadena y luego en numero nuevamente
        -- para las operaciones
        InsertarUnoAlprincipio ->
            if calcula.calculado == 0 then
                { calcula | calculado = 1 }

            else
                { calcula | calculado = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.calculado ++ "1")) }

        InsertarDosAlprincipio ->
            if calcula.calculado == 0 then
            { calcula | calculado = 2 }
            else
                { calcula | calculado = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.calculado ++ "2")) }

        InsertarTresAlprincipio ->
            if calcula.calculado == 0 then
            { calcula | calculado = 3 }
            else
                { calcula | calculado = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.calculado ++ "3")) }

        InsertarCuatroAlprincipio ->
            if calcula.calculado == 0 then
            { calcula | calculado = 4 }
            else
                { calcula | calculado = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.calculado ++ "4")) }

        InsertarCincoAlprincipio ->
            if calcula.calculado == 0 then
            { calcula | calculado = 5 }
            else
                { calcula | calculado = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.calculado ++ "5")) }

        InsertarSeisAlprincipio ->
            if calcula.calculado == 0 then
            { calcula | calculado = 6 }
            else
                { calcula | calculado = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.calculado ++ "6")) }

        InsertarSieteAlprincipio ->
            if calcula.calculado == 0 then
            { calcula | calculado = 7 }
            else
                { calcula | calculado = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.calculado ++ "7")) }

        InsertarOchoAlprincipio ->
            if calcula.calculado == 0 then
            { calcula | calculado = 8 }
            else
                { calcula | calculado = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.calculado ++ "8")) }

        InsertarNueveAlprincipio ->
            if calcula.calculado == 0 then
            { calcula | calculado = 9 }
            else
                { calcula | calculado = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.calculado ++ "9")) }

        -- Insertado de numeros
        -- De todos los valores insertados, se evaluan si son iguales a cero
        -- Sino es el caso transforma el valor anterior en cadena y luego en numero nuevamente
        -- para las operaciones
        InsertarUno ->
            if calcula.numero == 0 then
                { calcula | numero = 1 }

            else
                { calcula | numero = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.numero ++ "1")) }

        InsertarDos ->
            if calcula.numero == 0 then
                { calcula | numero = 2 }

            else
                { calcula | numero = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.numero ++ "2")) }

        InsertarTres ->
            if calcula.numero == 0 then
                { calcula | numero = 3 }

            else
                { calcula | numero = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.numero ++ "3")) }

        InsertarCuatro ->
            if calcula.numero == 0 then
                { calcula | numero = 4 }

            else
                { calcula | numero = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.numero ++ "4")) }

        InsertarCinco ->
            if calcula.numero == 0 then
                { calcula | numero = 5 }

            else
                { calcula | numero = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.numero ++ "5")) }

        InsertarSeis ->
            if calcula.numero == 0 then
                { calcula | numero = 6 }

            else
                { calcula | numero = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.numero ++ "6")) }

        InsertarSiete ->
            if calcula.numero == 0 then
                { calcula | numero = 7 }

            else
                { calcula | numero = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.numero ++ "7")) }

        InsertarOcho ->
            if calcula.numero == 0 then
                { calcula | numero = 8 }

            else
                { calcula | numero = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.numero ++ "8")) }

        InsertarNueve ->
            if calcula.numero == 0 then
                { calcula | numero = 9 }

            else
                { calcula | numero = Maybe.withDefault 0 (String.toInt (String.fromInt calcula.numero ++ "9")) }

        -- Vista de operadores
        -- Agrega el operador con el que determina el calculo
        InsertarVSuma ->
            { calcula | operador = "+" }

        InsertarVResta ->
            { calcula | operador = "-" }

        InsertarVMulti ->
            { calcula | operador = "*" }

        InsertarVDiv ->
            { calcula | operador = "/" }



-- Vista de la App (vista final)


vista : Calcular -> Html Mensaje
vista resultado =
    div
        [ -- Estilos para Centrar toda la pagina
          style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "flex-direction" "column"
        , style "margin-top" "40px"
        ]
        -- Estilos para el panel
        [ div
            [ style "background-color" "#ecf8f8"
            , style "padding" "10px 8em"
            , style "border" "solid 1px"
            , style "border-radius" "5px"
            ]
            [ text (panelDeCalculo resultado.calculado resultado.operador resultado.numero) ]
        , div []
            [ btnCalc InsertarVSuma "+"
            , btnCalc InsertarVResta "-"
            , btnCalc InsertarVMulti "×"
            , btnCalc InsertarVDiv "÷"
            ]
        , div []
            [ btnCalc Reinicio "Reinicio" ]
        , insercionNumeros resultado.operador
        , div [] [ btnIgualdad resultado.operador ]
        ]


-- Componentes de la vista


-- Evalua si el operador esta declarado para insertar el segundo numero
-- de la operacion matematica
insercionNumeros : String -> Html Mensaje
insercionNumeros operador =
    if String.isEmpty operador then
        -- Botones para el primer numero
        div []
            [ btnCalc InsertarUnoAlprincipio "1"
            , btnCalc InsertarDosAlprincipio "2"
            , btnCalc InsertarTresAlprincipio "3"
            , btnCalc InsertarCuatroAlprincipio "4"
            , btnCalc InsertarCincoAlprincipio "5"
            , btnCalc InsertarSeisAlprincipio "6"
            , btnCalc InsertarSieteAlprincipio "7"
            , btnCalc InsertarOchoAlprincipio "8"
            , btnCalc InsertarNueveAlprincipio "9"
            ]

    else
        -- Botones para el segundo numero
        div []
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
        , style "padding" "10px 4em"
        , style "border-radius" "6px"
        , style "font-weight" "700"
        , style "margin-top" "20px"
        ]
        [ text "=" ]


-- El boton de igualdad determina el operador y la funcion a utilizar
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
        btnIgualDesactivado

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
        , style "padding" "10px 4em"
        , style "border-radius" "6px"
        , style "font-weight" "700"
        , style "margin-top" "20px"
        ]
        [ text contenido ]
