module Utility.DateTime exposing
    ( dateFromNaiveDateTime
    , daysBetweenNaiveDates
    , isoStringFromNaiveDateTime
    , offsetDateTimeStringByHours
    , posixFromNaiveDateString
    , rataDieFromNaiveDateTime
    )

import Date exposing (Date, Unit(..), diff)
import Parser exposing (..)
import Time


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


dateFromNaiveDateTime : String -> Result String Date
dateFromNaiveDateTime str =
    str
        |> isoStringFromNaiveDateTime
        |> Result.fromMaybe "error parsing date string"
        |> Result.andThen Date.fromIsoString


rataDieFromNaiveDateTime : String -> Int
rataDieFromNaiveDateTime dateString =
    case dateFromNaiveDateTime dateString of
        Ok date ->
            Date.toRataDie date

        Err _ ->
            0


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
