module Utility.TypedTime exposing
    ( TypedTime(..)
    , Unit(..)
    , convertScalarToSecondsWithUnit
    , convertTo
    , decodeHM
    , multiply
    , timeAsStringWithUnit
    )

import Parser exposing (..)


type Unit
    = Seconds
    | Minutes
    | Hours


type TypedTime
    = TypedTime Unit Float


multiply : Float -> TypedTime -> TypedTime
multiply f (TypedTime unit value) =
    TypedTime unit (f * value)


convertScalarToSecondsWithUnit : Unit -> Float -> Float
convertScalarToSecondsWithUnit inputUnit scalar =
    let
        inputValue =
            TypedTime inputUnit scalar
    in
    convertToSeconds inputValue


convertToSeconds : TypedTime -> Float
convertToSeconds tt =
    case tt of
        TypedTime Seconds s ->
            s

        TypedTime Minutes m ->
            60 * m

        TypedTime Hours h ->
            3600 * h


convertFromSecondsWithUnit : Unit -> Float -> TypedTime
convertFromSecondsWithUnit unit s =
    case unit of
        Seconds ->
            TypedTime Seconds s

        Minutes ->
            TypedTime Minutes (s / 60)

        Hours ->
            TypedTime Hours (s / 3600)


convertTo : Unit -> TypedTime -> TypedTime
convertTo unit tt =
    convertToSeconds tt
        |> convertFromSecondsWithUnit unit


timeAsStringWithUnit : Unit -> TypedTime -> String
timeAsStringWithUnit unit tt =
    let
        (TypedTime unit_ value) =
            convertTo Seconds tt
    in
    case unit of
        Seconds ->
            hmsStringFromSeconds value

        Minutes ->
            hmStringFromSeconds value

        Hours ->
            "hhh"


type alias HMSRecord =
    { seconds : Int, minutes : Int, hours : Int }


type alias HMRecord =
    { hours : Int, minutes : Int }


hmsRecordFromSeconds : Float -> HMSRecord
hmsRecordFromSeconds s =
    let
        s1 =
            round s

        s2 =
            modBy 60 s1

        m1 =
            s1 // 60

        m2 =
            modBy 60 m1

        h1 =
            m1 // 60
    in
    { seconds = s2, minutes = m2, hours = h1 }


hmsStringFromSeconds : Float -> String
hmsStringFromSeconds s =
    let
        tr =
            hmsRecordFromSeconds s
    in
    (String.padLeft 2 '0' <| String.fromInt tr.hours)
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt tr.minutes)
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt tr.seconds)



---
---


hmRecordFromSeconds : Float -> HMRecord
hmRecordFromSeconds s =
    let
        m1 =
            round (s / 60)

        m2 =
            modBy 60 m1

        h1 =
            m1 // 60
    in
    { minutes = m2, hours = h1 }


hmStringFromSeconds : Float -> String
hmStringFromSeconds s =
    let
        tr =
            hmRecordFromSeconds s
    in
    (String.padLeft 2 '0' <| String.fromInt tr.hours)
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt tr.minutes)



--
-- TIME PARSER
--


decodeHM : String -> Maybe Float
decodeHM str =
    Parser.run parseTime str |> Result.toMaybe


{-| run parseHM takes a string in the format hh:mm as
input and returns the resulting time in seconds.

> run parseHM "02:10"

    Ok 7800 : Result (List DeadEnd) Float

-}
parseHM : Parser Float
parseHM =
    (succeed HMRecord
        |= parseInt
        |. symbol ":"
        |= parseInt
    )
        |> Parser.map (\hm -> hm.hours * 3600 + hm.minutes * 60 |> toFloat)


parseTime : Parser Float
parseTime =
    oneOf [ backtrackable parseHM, float |> Parser.map (\x -> 60 * x) ]


parseInt_ : Parser Int
parseInt_ =
    succeed identity
        |. symbol "0"
        |= int


parseInt : Parser Int
parseInt =
    oneOf [ parseInt_, int ]
