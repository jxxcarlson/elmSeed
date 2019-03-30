port module OutsideInfo exposing
    ( InfoForElm(..)
    , InfoForOutside(..)
    , getInfoFromOutside
    , sendInfoOutside
    , userDecoderForOutside
    , userEncoder
    )

import Common.Utility as Utility
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import User.Types exposing (User)


port infoForOutside : GenericOutsideData -> Cmd msg


port infoForElm : (GenericOutsideData -> msg) -> Sub msg


type InfoForOutside
    = UserData Encode.Value
    | AskToReconnectUser Encode.Value
    | DisconnectUser Encode.Value


type alias GenericOutsideData =
    { tag : String, data : Encode.Value }


type InfoForElm
    = LocalStorageInfo User


sendInfoOutside : InfoForOutside -> Cmd msg
sendInfoOutside info =
    case info of
        UserData value ->
            infoForOutside { tag = "UserData", data = value }

        AskToReconnectUser value ->
            infoForOutside { tag = "AskToReconnectUser", data = value }

        DisconnectUser value ->
            infoForOutside { tag = "DisconnectUser", data = value }


getInfoFromOutside : (InfoForElm -> msg) -> (String -> msg) -> Sub msg
getInfoFromOutside tagger onError =
    infoForElm
        (\outsideInfo ->
            case outsideInfo.tag of
                "ReconnectUser" ->
                    case Decode.decodeValue userDecoderForOutside outsideInfo.data of
                        Ok localStorageRecord ->
                            tagger <| LocalStorageInfo localStorageRecord

                        Err e ->
                            onError "Error reconnecting user"

                _ ->
                    onError <| "Unexpected info from outside: "
        )


userDecoderForOutside : Decode.Decoder User
userDecoderForOutside =
    Decode.succeed User
        |> required "username" Decode.string
        |> required "id" (Decode.map stringToInt Decode.string)
        |> required "email" Decode.string
        |> required "token" Decode.string
        |> required "admin" (Decode.map stringToBool Decode.string)
        |> required "verified" (Decode.map stringToBool Decode.string)


statListStringToReadingStatList : String -> List ReadingStat
statListStringToReadingStatList str =
    str
        |> String.split ","
        |> List.map statStringToReadingStat
        |> Utility.maybeExtraValues


statStringToReadingStat : String -> Maybe ReadingStat
statStringToReadingStat str =
    let
        parts =
            String.split ":" str

        datePart =
            List.head parts

        pagesReadPart =
            parts |> List.tail |> Maybe.andThen List.head
    in
    case ( datePart, pagesReadPart ) of
        ( Just dateString, Just pagesReadString ) ->
            Just { dateString = dateString, pagesRead = String.toInt pagesReadString |> Maybe.withDefault 0 }

        ( _, _ ) ->
            Nothing


type alias ReadingStat =
    { dateString : String
    , pagesRead : Int
    }


userEncoder : User -> Encode.Value
userEncoder user =
    Encode.object
        [ ( "username", Encode.string user.username )
        , ( "id", Encode.string (String.fromInt user.id) )
        , ( "email", Encode.string user.email )
        , ( "token", Encode.string user.token )
        , ( "admin", Encode.string (boolToString user.admin) )
        , ( "verified", Encode.string (boolToString user.verified) )
        ]


statStringValue : ReadingStat -> String
statStringValue stat =
    stat.dateString ++ ":" ++ String.fromInt stat.pagesRead


statListStringValue : List ReadingStat -> String
statListStringValue stats =
    stats
        |> List.map statStringValue
        |> String.join ","


stringToBool : String -> Bool
stringToBool str =
    if str == "True" then
        True

    else
        False


boolToString : Bool -> String
boolToString b =
    case b of
        True ->
            "True"

        False ->
            "False"


stringToInt : String -> Int
stringToInt str =
    str |> String.toInt |> Maybe.withDefault 0


stringToList : String -> List String
stringToList str =
    str |> String.words |> List.map normalize


normalize : String -> String
normalize str =
    if String.endsWith "," str then
        String.dropRight 1 str

    else
        str
