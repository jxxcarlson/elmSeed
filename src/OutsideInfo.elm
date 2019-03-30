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
import User.Types exposing (PublicUser, User)


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
        |> required "firstname" Decode.string
        |> required "email" Decode.string
        |> required "token" Decode.string
        |> required "blurb" Decode.string
        |> required "public" (Decode.map stringToBool Decode.string)
        |> required "follow" (Decode.string |> Decode.map stringToPublicUserList)
        |> required "followers" (Decode.string |> Decode.map stringToPublicUserList)
        |> required "admin" (Decode.map stringToBool Decode.string)
        |> required "beginningDate" Decode.string
        |> required "tags" (Decode.string |> Decode.map (String.split ","))
        |> required "reading_stats" (Decode.string |> Decode.map statListStringToReadingStatList)
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
        , ( "firstname", Encode.string user.firstname )
        , ( "email", Encode.string user.email )
        , ( "token", Encode.string user.token )
        , ( "blurb", Encode.string user.blurb )
        , ( "public", Encode.string (boolToString user.public) )
        , ( "follow", Encode.string (publicUserListToString user.follow) )
        , ( "followers", Encode.string (publicUserListToString user.followers) )
        , ( "admin", Encode.string (boolToString user.admin) )
        , ( "beginningDate", Encode.string user.beginningDate )
        , ( "tags", Encode.string (user.tags |> String.join ",") )
        , ( "reading_stats", Encode.string (statListStringValue user.readingStats) )
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


publicUserListToString : List PublicUser -> String
publicUserListToString publicUserList =
    publicUserList
        |> List.map .username
        |> String.join ","


stringToPublicUserList : String -> List PublicUser
stringToPublicUserList str =
    str
        |> String.split ","
        |> List.map String.trim
        |> List.map (\name -> { username = name })


publicUserDecoder : Decode.Decoder PublicUser
publicUserDecoder =
    Decode.succeed PublicUser
        |> required "username" Decode.string


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
