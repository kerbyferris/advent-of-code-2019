module Help exposing (log, test)

import Html exposing (Html, div, text)


test : int -> int -> Html msg
test expect actual =
    if expect == actual then
        div [] [ text "True" ]

    else
        div [] [ text ("False. Actual: " ++ Debug.toString actual) ]


log res =
    div [] [ text ("Result: " ++ Debug.toString res) ]
