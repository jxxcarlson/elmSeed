module Utility.TypedTime exposing
    ( TypedTime(..)
    , Unit(..)
    , convertTo
    , timeAsStringWithUnit
    )


type Unit
    = Seconds
    | Minutes
    | Hours


type TypedTime
    = TypedTime Unit Float


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
    { minutes : Int, hours : Int }


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
