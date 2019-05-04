module Utility.DateTime exposing
    ( dateFromNaiveDateTime
    , dateFromNaiveDateTime2
    , daysBetweenNaiveDates
    , humanDateFromNaiveDateTime
    , inLastNDaysBeforeDate
    , isoStringFromNaiveDateTime
    , naiveDateStringFromPosix
    , offsetDateTimeStringByHours
    , posixFromNaiveDateString
    , rataDieFromNaiveDateTime
    )

import Date exposing (Date, Unit(..), diff)
import Logger.Scalar exposing (NaiveDateTime(..))
import Parser exposing (..)
import Time exposing (Posix)


type alias TR =
    { hour : Int
    , minute : Int
    , second : Int
    }


parseTR : Parser TR
parseTR =
    succeed TR
        |= (parseUntil ':' |> Parser.map strToInt)
        |. symbol ":"
        |. chompWhile (\c -> c == '0')
        |= (parseUntil ':' |> Parser.map strToInt)
        |. symbol ":"
        |. chompWhile (\c -> c == '0')
        |= (parseUntil '.' |> Parser.map strToInt)


offsetTRByHours : Int -> TR -> TR
offsetTRByHours h tt =
    let
        newHours =
            tt.hour + h
    in
    if newHours >= 24 then
        { tt | hour = newHours - 24 }

    else if newHours > 0 then
        { tt | hour = newHours }

    else
        { tt | hour = newHours + 24 }


offsetDateTimeStringByHours : Int -> String -> String
offsetDateTimeStringByHours k str =
    let
        tts =
            case String.split "T" str of
                [ _, tts_ ] ->
                    tts_

                _ ->
                    ""

        posix_ =
            posixfromNaiveDateTimeString str

        shiftedDateString =
            Date.fromPosix Time.utc (Time.millisToPosix ((posix_ + k * 3600) * 1000)) |> Date.toIsoString

        shiftedTimeString =
            case run parseTR tts of
                Ok tr ->
                    offsetTRByHours k tr |> stringValueOfTR

                Err _ ->
                    "00:00:00"
    in
    shiftedDateString ++ "T" ++ shiftedTimeString


stringValueOfTR : TR -> String
stringValueOfTR tr =
    let
        h =
            String.padLeft 2 '0' <| String.fromInt tr.hour

        min =
            String.padLeft 2 '0' <| String.fromInt tr.minute

        s =
            String.padLeft 2 '0' <| String.fromInt tr.second
    in
    String.join ":" [ h, min, s ]


strToInt : String -> Int
strToInt str =
    str
        |> String.toInt
        |> Maybe.withDefault 0


parseUntil : Char -> Parser String
parseUntil c =
    getChompedString <|
        succeed ()
            |. chompWhile (\c_ -> c_ /= c)


isoStringFromNaiveDateTime : String -> Maybe String
isoStringFromNaiveDateTime str =
    String.split "T" str
        |> List.head


humanDateFromNaiveDateTime : NaiveDateTime -> String
humanDateFromNaiveDateTime (NaiveDateTime str) =
    str
        |> dateFromNaiveDateTime2
        |> Maybe.map (Date.format "EE, MMM d")
        |> Maybe.withDefault ""


dateFromNaiveDateTime : String -> Result String Date
dateFromNaiveDateTime str =
    str
        |> isoStringFromNaiveDateTime
        |> Result.fromMaybe "error parsing date string"
        |> Result.andThen Date.fromIsoString


dateFromNaiveDateTime2 : String -> Maybe Date
dateFromNaiveDateTime2 str =
    str
        |> isoStringFromNaiveDateTime
        |> Result.fromMaybe "error parsing date string"
        |> Result.andThen Date.fromIsoString
        |> Result.toMaybe


rataDieFromNaiveDateTime : String -> Int
rataDieFromNaiveDateTime dateString =
    case dateFromNaiveDateTime dateString of
        Ok date ->
            Date.toRataDie date

        Err _ ->
            0


inLastNDaysBeforeDate : String -> Int -> String -> Maybe Bool
inLastNDaysBeforeDate endDate interval runningDate =
    let
        predicate =
            \x -> x <= interval
    in
    Maybe.map predicate (daysBetweenNaiveDates runningDate endDate)


daysBetweenNaiveDates : String -> String -> Maybe Int
daysBetweenNaiveDates dateString1 dateString2 =
    let
        d1_ =
            dateFromNaiveDateTime dateString1

        d2_ =
            dateFromNaiveDateTime dateString2
    in
    case ( d1_, d2_ ) of
        ( Ok d1, Ok d2 ) ->
            Just <| diff Days d1 d2

        ( _, _ ) ->
            Nothing


naiveDateStringFromPosix : Posix -> String
naiveDateStringFromPosix posix =
    let
        y =
            Time.toYear Time.utc posix |> String.fromInt

        m =
            Time.toMonth Time.utc posix |> monthToString

        d =
            Time.toDay Time.utc posix |> String.fromInt |> String.padLeft 2 '0'
    in
    y ++ "-" ++ m ++ "-" ++ d


monthToString : Time.Month -> String
monthToString m =
    case m of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"


posixFromNaiveDateString str =
    case dateFromNaiveDateTime str of
        Err _ ->
            0

        Ok result ->
            (Date.toRataDie result - 719163) * 86400


posixFromTimeString str =
    case run parseTR str of
        Err _ ->
            0

        Ok result ->
            let
                h =
                    result.hour * 3600

                m =
                    result.minute * 60

                s =
                    result.second
            in
            h + m + s


posixfromNaiveDateTimeString str =
    case String.split "T" str of
        [ dd, tt ] ->
            posixFromNaiveDateString dd + posixFromTimeString tt

        _ ->
            0
