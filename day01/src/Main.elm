module Main exposing (main)

import Data exposing (data)
import Help exposing (log, test)
import Html exposing (Html, div, text)
import List exposing (map, sum)


fuelForMass : Int -> Int
fuelForMass mass =
    let
        res =
            mass // 3 - 2
    in
    if res >= 0 then
        res

    else
        0


fuelForFuel : Int -> Int -> Int
fuelForFuel fuelMass acc =
    let
        requiredFuel =
            fuelForMass fuelMass
    in
    if requiredFuel <= 0 then
        acc

    else
        fuelForFuel requiredFuel (acc + requiredFuel)


fuelRequired : Int -> Int
fuelRequired mass =
    let
        massFuel =
            fuelForMass mass
    in
    massFuel + fuelForFuel massFuel 0


main : Html msg
main =
    div []
        [ test (fuelForMass 12) 2
        , test (fuelForMass 1) 0
        , test (fuelForFuel 14 0) 2
        , test (fuelForFuel 1969 0) 966
        , test (fuelForFuel 100756 0) 50346
        , test (fuelForFuel 100756 0) 503406
        , test (fuelRequired 100756) 50346
        , log (sum (map fuelRequired data))
        ]
