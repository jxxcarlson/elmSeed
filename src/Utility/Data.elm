module Utility.Data exposing
    ( correctTimeZone
    , eventsByDay
    , filterValues
    , group
    , rataDie
    )

import List.Extra as LE
import Logger.Scalar exposing (NaiveDateTime(..))
import SharedState exposing (Event, Log)
import Utility.DateTime exposing (offsetDateTimeStringByHours)


e1 =
    { id = 1, value = "10.0", insertedAt = NaiveDateTime "2019-03-30T09:10:00" }


e2 =
    { id = 2, value = "20.0", insertedAt = NaiveDateTime "2019-03-30T10:10:00" }


e2b =
    { id = 2, value = "30.0", insertedAt = NaiveDateTime "2019-03-30T11:10:00" }


e3 =
    { id = 3, value = "2.0", insertedAt = NaiveDateTime "2019-03-31T06:10:00" }


e4 =
    { id = 3, value = "4.0", insertedAt = NaiveDateTime "2019-03-31T:10:00" }



--
--data =
--    [ e1, e2, e2b, e3, e4 ]


timeSeries : List Event -> List ( NaiveDateTime, Float )
timeSeries eventList =
    eventList
        |> List.map (\event -> ( event.insertedAt, String.toFloat event.value ))
        |> filterValues


timeSeriesRD : List ( NaiveDateTime, Float ) -> List ( Int, ( NaiveDateTime, Float ) )
timeSeriesRD listOfPairs =
    List.map augmentPair listOfPairs


augmentPair : ( NaiveDateTime, Float ) -> ( Int, ( NaiveDateTime, Float ) )
augmentPair ( ndt, f ) =
    ( rataDie ndt, ( ndt, f ) )


filterValues : List ( a, Maybe b ) -> List ( a, b )
filterValues =
    List.foldr foldrValues []


foldrValues : ( a, Maybe b ) -> List ( a, b ) -> List ( a, b )
foldrValues pair list =
    case pair of
        ( _, Nothing ) ->
            list

        ( item, Just v ) ->
            ( item, v ) :: list


group : List ( a, b ) -> List (List ( a, b ))
group list =
    list
        |> LE.groupWhile (\x y -> Tuple.first x == Tuple.first y)
        |> List.map (\( u, v ) -> u :: v)



--groupDataByDay : List Event -> List Event
--groupDataByDay list =
--    timeSeries data |> timeSeriesRD |> group |> List.map sumList2


offsetTimeZone : Int -> Event -> Event
offsetTimeZone offset event =
    let
        (NaiveDateTime str) =
            event.insertedAt
    in
    { event | insertedAt = NaiveDateTime (offsetDateTimeStringByHours offset str) }


eventsByDay : Int -> List Event -> List Event
eventsByDay timeZoneOffset list =
    list
        |> List.map (\r -> offsetTimeZone timeZoneOffset r)
        |> timeSeries
        |> timeSeriesRD
        |> group
        |> List.map sumList2


correctTimeZone : Int -> List Event -> List Event
correctTimeZone timeZoneOffset list =
    list
        |> List.map (\r -> offsetTimeZone timeZoneOffset r)



--
--  timeSeries data |>  timeSeriesRD |> group
--
-- sumList2 :  List ( Int, ( Logger.Scalar.NaiveDateTime, Float ) ) -> (Int, Logger.Scalar.NaiveDateTime, Float)


sumList2 :
    List ( Int, ( NaiveDateTime, Float ) )
    -> Event
sumList2 list =
    let
        head =
            List.head list

        index : Int
        index =
            Maybe.map Tuple.first head |> Maybe.withDefault -1

        tuple =
            Maybe.map Tuple.second head

        dt : NaiveDateTime
        dt =
            Maybe.map Tuple.first tuple |> Maybe.withDefault (NaiveDateTime "2000-01-010T00:00:00")

        sum : Float
        sum =
            List.map (Tuple.second >> Tuple.second) list |> List.sum
    in
    { id = index + 1, insertedAt = dt, value = String.fromFloat sum }


sumList : List ( Int, Float ) -> ( Maybe Int, Float )
sumList list =
    let
        sum =
            list |> List.map Tuple.second |> List.sum

        index =
            List.head list |> Maybe.map Tuple.first
    in
    ( index, sum )


rataDie : NaiveDateTime -> Int
rataDie (NaiveDateTime str) =
    Utility.DateTime.rataDieFromNaiveDateTime str - 737148
