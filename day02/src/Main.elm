module Main exposing (main)

import Array exposing (fromList, get, toIndexedList, toList)
import Data exposing (data)
import Help exposing (log, test)
import Html exposing (Html, div, text)
import List exposing (append, drop, length, map, sum, take)
import Maybe exposing (map2, withDefault)


type alias ReplaceData =
    { idx : Int
    , val : Int
    }


runProgram : List Int -> Int -> List Int
runProgram opCodes idx =
    let
        opCodeArray =
            fromList opCodes

        opCode =
            get idx opCodeArray

        val1 =
            withDefault -1 (get (withDefault -1 (get (idx + 1) opCodeArray)) opCodeArray)

        val2 =
            withDefault -1 (get (withDefault -1 (get (idx + 2) opCodeArray)) opCodeArray)

        position =
            withDefault -1 (get (idx + 3) opCodeArray)

        nextIdx =
            idx + 4
    in
    case opCode of
        Just 1 ->
            runProgram (replaceCode { idx = position, val = val1 + val2 } opCodes) nextIdx

        Just 2 ->
            runProgram (replaceCode { idx = position, val = val1 * val2 } opCodes) nextIdx

        Just 99 ->
            opCodes

        _ ->
            []


replaceCode : ReplaceData -> List Int -> List Int
replaceCode replace codes =
    -- TODO: maybe because idx could be out of range
    take replace.idx codes ++ [ replace.val ] ++ drop (replace.idx + 1) codes


replaceCodes : List ReplaceData -> List Int -> List Int
replaceCodes replacements codes =
    case replacements of
        [] ->
            codes

        [ x ] ->
            replaceCode x codes

        x :: xs ->
            replaceCodes xs (replaceCode x codes)


programRestore : List Int -> List Int
programRestore codes =
    replaceCodes [ { idx = 1, val = 12 }, { idx = 2, val = 2 } ] codes


programTest : List Int -> List Int
programTest codes =
    replaceCodes [ { idx = 1, val = 37 }, { idx = 2, val = 49 } ] codes


validateCodes : List Int -> Html msg
validateCodes codes =
    if length codes < 5 then
        div [] [ text "Invalid Input" ]
        -- TODO: validate every fourth element is a control code

    else
        div [] [ text (Debug.toString (runProgram (programRestore codes) 0)) ]


sampleInput : List Int
sampleInput =
    [ 1, 0, 0, 0, 99 ]


main : Html msg
main =
    div []
        [ log data
        , log (runProgram (programRestore data) 0)
        , log 19690720
        , log (runProgram (programTest data) 0)
        ]
