module Common.Utility exposing
    ( intListFromElixirDateString
    , listUpdateIf
    , maybeExtraValues
    , removeLeadingString
    , removeLeadingZeros
    , replaceIf
    , roundTo
    , scale
    , showIf
    , softBreakAt
    , stringFromIntWithCommas
    , toUtcDateString
    , toUtcString
    , toggleList
    , usDateStringFromElixirDateString
    )

import Common.Days as Days
import Element exposing (Element)
import Regex
import Time exposing (Month(..), Posix)


intListFromElixirDateString : String -> List Int
intListFromElixirDateString str =
    let
        maybeElixirDate =
            String.split "T" str |> List.head
    in
    case maybeElixirDate of
        Nothing ->
            []

        Just str_ ->
            String.split "-" str_
                |> List.map String.toInt
                |> List.map (Maybe.withDefault 0)


usDateStringFromElixirDateString : String -> String
usDateStringFromElixirDateString dateString =
    dateString
        |> intListFromElixirDateString
        |> Days.fromIntList
        |> Days.usDateStringFromDate


toUtcString : Maybe Posix -> String
toUtcString maybeTime =
    case maybeTime of
        Nothing ->
            "--:--:--"

        Just time ->
            (String.fromInt (Time.toHour Time.utc time) |> String.padLeft 2 '0')
                ++ ":"
                ++ (String.fromInt (Time.toMinute Time.utc time) |> String.padLeft 2 '0')



-- ++ ":"
-- ++ (String.fromInt (Time.toSecond Time.utc time) |> String.padLeft 2 '0')


toUtcDateString : Maybe Posix -> String
toUtcDateString maybeTime =
    case maybeTime of
        Nothing ->
            "--/--/--"

        Just time ->
            String.fromInt (monthToInt <| Time.toMonth Time.utc time)
                ++ "/"
                ++ String.fromInt (Time.toDay Time.utc time)
                ++ "/"
                ++ String.fromInt (Time.toYear Time.utc time)


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


{-| Replace all values that satisfy a predicate with a replacement value.
-}
replaceIf : (a -> Bool) -> a -> List a -> List a
replaceIf predicate replacement list =
    updateIf predicate (always replacement) list


{-| Replace all values that satisfy a predicate by calling an update function.
-}
updateIf : (a -> Bool) -> (a -> a) -> List a -> List a
updateIf predicate update list =
    List.map
        (\item ->
            if predicate item then
                update item

            else
                item
        )
        list


{-| Remove the first occurrence of an element of a list
-}
remove : a -> List a -> List a
remove x xs =
    case xs of
        [] ->
            []

        y :: ys ->
            if x == y then
                ys

            else
                y :: remove x ys


{-| toggleList a list removes a from list if a is in the list.
If it is not in the list, then it prepends it
-}
toggleList : a -> List a -> List a
toggleList x xs =
    if List.member x xs then
        remove x xs

    else
        x :: xs


softBreak : Int -> String -> List String
softBreak width string =
    if width <= 0 then
        []

    else
        string
            |> Regex.find (softBreakRegexp width)
            |> List.map .match



-- softBreakLongLines : Int -> String -> List String
-- softBreakLongLines width string =
--   string
--     |> String.lines
--     |> String.


softBreakAtAux : Int -> String -> List String
softBreakAtAux width string =
    if String.length string < width || String.contains "http" string then
        [ string ]

    else
        softBreak width string


softBreakAtAux2 : Int -> String -> List String
softBreakAtAux2 width string =
    if String.length string < width then
        [ string ]

    else if String.contains "http" string then
        let
            n =
                String.length (getUrl string)
        in
        case n > 0 of
            False ->
                [ string ]

            True ->
                softBreak (width - n + 10) string

    else
        softBreak width string


getUrl : String -> String
getUrl str =
    str
        |> String.words
        |> List.filter (\w -> String.contains "http" w)
        |> List.head
        |> Maybe.withDefault ""


softBreakAt : Int -> String -> List String
softBreakAt width string =
    string
        |> String.lines
        |> List.map (softBreakAtAux width)
        |> flattenListList


flattenListList : List (List String) -> List String
flattenListList stringListList =
    List.map flattenList stringListList


flattenList : List String -> String
flattenList stringList =
    let
        n =
            List.length stringList
    in
    if n < 2 then
        List.head stringList |> Maybe.withDefault ""

    else
        stringList |> String.join "\n"


softBreakRegexp : Int -> Regex.Regex
softBreakRegexp width =
    Maybe.withDefault Regex.never <|
        Regex.fromString (".{1," ++ String.fromInt width ++ "}(\\s+|$)|\\S+?(\\s+|$)")


roundTo : Int -> Float -> Float
roundTo k x =
    let
        kk =
            toFloat k
    in
    x * 10.0 ^ kk |> round |> toFloat |> (\y -> y / 10.0 ^ kk)


stringFromIntWithCommas : Int -> String
stringFromIntWithCommas x =
    let
        i =
            modBy 1000 x

        j_ =
            x // 1000

        j =
            modBy 1000 j_

        k_ =
            j_ // 1000
    in
    [ k_, j, i ]
        |> removeLeadingZeros
        |> List.map String.fromInt
        |> List.map (String.padLeft 3 '0')
        |> String.join ","
        |> removeLeadingString "0"


removeLeadingZeros list =
    case List.head list of
        Nothing ->
            []

        Just 0 ->
            removeLeadingZeros (List.tail list |> Maybe.withDefault [])

        Just _ ->
            list


removeLeadingString : String -> String -> String
removeLeadingString s str =
    if String.startsWith s str then
        removeLeadingString s (String.dropLeft (String.length s) str)

    else
        str


maybeExtraValues : List (Maybe a) -> List a
maybeExtraValues =
    List.foldr foldrValues []


foldrValues : Maybe a -> List a -> List a
foldrValues item list =
    case item of
        Nothing ->
            list

        Just v ->
            v :: list


showIf : Bool -> Element msg -> Element msg
showIf flag element =
    if flag then
        element

    else
        Element.none


{-| from elm-community/list-extra
-}
listUpdateIf : (a -> Bool) -> (a -> a) -> List a -> List a
listUpdateIf predicate update list =
    List.map
        (\item ->
            if predicate item then
                update item

            else
                item
        )
        list


scale : Float -> Int -> Int
scale factor k =
    round <| factor * toFloat k
