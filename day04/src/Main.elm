module Main exposing (main)

import Array exposing (fromList, get, toIndexedList, toList)
import Bool.Extra exposing (all)
import Help exposing (log, test)
import Html exposing (Html, div, text)
import List exposing (append, drop, filter, head, length, map, member, minimum, range, sum, take)
import Maybe exposing (map2, withDefault)
import Set exposing (intersect)
import String exposing (dropLeft, left, split, toInt)
import Tuple exposing (first, pair, second)


data =
    range 168630 718098


intToDigits : Int -> List Int
intToDigits password =
    map (\x -> withDefault -1 (toInt x)) (split "" (Debug.toString password))


validateDigitsNeverDecrease : List Int -> Bool
validateDigitsNeverDecrease digits =
    case digits of
        [] ->
            True

        [ d ] ->
            True

        d :: ds ->
            if withDefault False (map2 (<=) (Just d) (head ds)) then
                validateDigitsNeverDecrease ds

            else
                False


validateDigitsRepeatOnce : List Int -> Bool
validateDigitsRepeatOnce digits =
    case digits of
        [] ->
            False

        [ d ] ->
            False

        d :: ds ->
            if Just d == head ds then
                if Just d == head (drop 1 ds) then
                    validateDigitsRepeatOnce (dropUntilChange ds d)

                else
                    True

            else
                validateDigitsRepeatOnce ds


dropUntilChange : List Int -> Int -> List Int
dropUntilChange digits compare =
    case digits of
        [] ->
            []

        [ d ] ->
            if Just compare == Just d then
                []

            else
                [ d ]

        d :: ds ->
            if Just compare == Just d then
                dropUntilChange ds compare

            else
                digits


validate : Int -> Bool
validate password =
    let
        passwordAsDigits =
            intToDigits password
    in
    all
        [ validateDigitsNeverDecrease passwordAsDigits
        , validateDigitsRepeatOnce passwordAsDigits
        ]


runProgram input =
    length (filter validate input)


main : Html msg
main =
    div []
        [ log (runProgram data)
        ]
