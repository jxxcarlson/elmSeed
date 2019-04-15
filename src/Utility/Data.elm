module Utility.Data exposing
    ( correctTimeZone
    , eventsByDay
    , fillGaps
    , filterValues
    , group
    , rataDie
    )

import List.Extra as LE
import Logger.Scalar exposing (NaiveDateTime(..))
import Maybe.Extra
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
        |> List.map (\event -> ( event.insertedAt, event.value ))


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
        |> List.sortBy Tuple.first
        |> fillGaps ( NaiveDateTime "1900-01-01", 0 )
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
    { id = index + 1, insertedAt = dt, value = sum }


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


{-|

> import FillGaps
> FillGaps.fillGaps "x" [(0,"a"),(0,"aa"),(2,"c"),(4,"e")][(0,"a"),(0,"aa"),(1,"x"),(2,"c"),(3,"x"),(4,"e")]

Actually, needed to start acc at `(-1 , [])` if there's no leading zeros:

    > [(0,"x"),(1,"x"),(2,"c"),(3,"x"),(4,"e")]```
    And `fillGap` was missing a call to reverse I just edited in.

    Folds are fun. You can keep as much state around as you want for the intermediate calculations and just throw it away when you're done

-}
fillGaps1 : a -> List ( Int, a ) -> List ( Int, a )
fillGaps1 default =
    let
        fillGap start end =
            List.range (start + 1) (end - 1)
                |> List.map (\i -> ( i, default ))
    in
    List.reverse
        << Tuple.second
        << List.foldl
            (\(( i, a ) as item) ( last, acc ) ->
                if i == last then
                    ( last, item :: acc )

                else if i == last + 1 then
                    ( last + 1, item :: acc )

                else
                    ( i, item :: fillGap last i ++ acc )
            )
            ( 0, [] )


fillGaps : a -> List ( Int, a ) -> List ( Int, a )
fillGaps default list =
    let
        fillGap start end =
            List.range (start + 1) (end - 1)
                |> List.map (\i -> ( i, default ))

        ii : Int
        ii =
            Maybe.map Tuple.first (List.head list) |> Maybe.withDefault 0
    in
    List.foldl
        (\(( i, a ) as item) ( last, acc ) ->
            if i == last then
                ( last, item :: acc )

            else if i == last + 1 then
                ( last + 1, item :: acc )

            else
                ( i, item :: fillGap last i ++ acc )
        )
        ( ii, [] )
        list
        |> Tuple.second
        |> List.reverse
