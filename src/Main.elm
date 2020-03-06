module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
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
            round (toFloat calcula / 2)



-- Vista de la App


vista : Calcular -> Html Mensaje
vista resultado =
    div []
        [ div [] [ text (String.fromInt resultado) ]
        , button [ onClick Suma ] [ text "+" ]
        , button [ onClick Resta ] [ text "-" ]
        , button [ onClick Multiplica ] [ text "*2" ]
        , button [ onClick Divide ] [ text "/2" ]
        ]
