module Main exposing (main)

import Array exposing (fromList, get, toIndexedList, toList)
import Data exposing (data)
import Data2 exposing (data2)
import Help exposing (log, test)
import Html exposing (Html, div, text)
import List exposing (append, drop, filter, length, map, member, minimum, range, sum, take)
import Maybe exposing (map2, withDefault)
import Set exposing (intersect)
import String exposing (dropLeft, left, toInt)
import Tuple exposing (first, pair, second)


type Direction
    = Right
    | Left
    | Up
    | Down


type alias Move =
    { direction : Direction, steps : Int }


type alias Position =
    ( Int, Int )


type alias Trail =
    List Position


parseMove : String -> Move
parseMove strMove =
    let
        dir =
            left 1 strMove

        numSteps =
            withDefault 0 (toInt (dropLeft 1 strMove))
    in
    case dir of
        "R" ->
            { direction = Right, steps = numSteps }

        "L" ->
            { direction = Left, steps = numSteps }

        "U" ->
            { direction = Up, steps = numSteps }

        "D" ->
            { direction = Down, steps = numSteps }

        _ ->
            { direction = Right, steps = numSteps }


getCrossings : Trail -> Trail -> Trail
getCrossings trail1 trail2 =
    Set.toList (intersect (Set.fromList trail1) (Set.fromList trail2))


getClosestCrossing : Trail -> Maybe Int
getClosestCrossing crossings =
    minimum (map getDistance crossings)


getDistance : Position -> Int
getDistance position =
    abs (first position) + abs (second position)


getMoves : Move -> Position -> List Position
getMoves move startPosition =
    let
        steps =
            range 1 move.steps
    in
    case move.direction of
        Right ->
            map (\x -> pair (x + first startPosition) (second startPosition)) steps

        Left ->
            map (\x -> pair (first startPosition - x) (second startPosition)) steps

        Up ->
            map (\y -> pair (first startPosition) (y + second startPosition)) steps

        Down ->
            map (\y -> pair (first startPosition) (second startPosition - y)) steps


getTrail : Trail -> List Move -> Position -> Trail
getTrail trailHerstory moveData currentPosition =
    case moveData of
        [] ->
            trailHerstory

        [ move ] ->
            trailHerstory ++ getMoves move currentPosition

        move :: moves ->
            let
                trail =
                    getMoves move currentPosition

                trailLength =
                    length trail

                endPosition =
                    withDefault ( 0, 0 ) (get 0 (fromList (drop (trailLength - 1) trail)))
            in
            getTrail (trailHerstory ++ trail) moves endPosition


stepsToCrossing : Trail -> Position -> Int -> Int
stepsToCrossing trail crossing acc =
    case trail of
        [] ->
            -1

        [ position ] ->
            if position == crossing then
                acc + 1

            else
                -1

        position :: positions ->
            if position == crossing then
                acc + 1

            else
                stepsToCrossing positions crossing (acc + 1)


runProgram strData =
    let
        trail1 =
            getTrail [] (map parseMove (first strData)) ( 0, 0 )

        trail2 =
            getTrail [] (map parseMove (second strData)) ( 0, 0 )

        crossings =
            getCrossings trail1 trail2
    in
    minimum (map (\x -> stepsToCrossing trail1 x 0 + stepsToCrossing trail2 x 0) crossings)


dataSample =
    [ "R3", "U1", "L1" ]


sampleMoves =
    map parseMove dataSample


sampleTrail =
    getTrail [] sampleMoves ( 0, 0 )


main : Html msg
main =
    div []
        [ log dataSample
        , log sampleMoves
        , log sampleTrail
        , log data2
        , log (stepsToCrossing sampleTrail ( 2, 1 ) 0)
        , log (runProgram data)

        -- , log (runProgram data)
        ]
