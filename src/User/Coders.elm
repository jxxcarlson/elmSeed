module User.Coders exposing
    ( annotatedUserDecoder
    , annotatedUserListDecoder
    , authorizationEncoder
    , publicUserDecoder
    , publicUserListDecoder
    , registrationEncoder
    , statusDecoder
    , tokenEncoder
    , userDecoder
    , userEncoder
    , userRecordEncoder
    )

import Common.Utility as Utility
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import User.Types exposing (AnnotatedUser, PublicUser, User, UserRecord)


publicUserDecoder : Decoder PublicUser
publicUserDecoder =
    Decode.succeed PublicUser
        |> required "username" Decode.string


publicUserListDecoder : Decoder (List PublicUser)
publicUserListDecoder =
    Decode.field "data" (Decode.list publicUserDecoder)


userEncoder : User -> Encode.Value
userEncoder user =
    Encode.object
        [ ( "username", Encode.string user.username )
        , ( "id", Encode.int user.id )
        , ( "firstname", Encode.string user.firstname )
        , ( "email", Encode.string user.email )
        , ( "token", Encode.string user.token )
        , ( "blurb", Encode.string user.blurb )
        , ( "public", Encode.bool user.public )
        , ( "follow", followEncoder user.follow )
        , ( "followers", followEncoder user.followers )
        , ( "admin", Encode.bool user.admin )
        , ( "beginningDate", Encode.string user.beginningDate )
        , ( "tags", Encode.list Encode.string user.tags )
        , ( "reading_stats", Encode.list encodeStat user.readingStats )
        , ( "verified", Encode.bool user.verified )
        ]


encodeStat : ReadingStat -> Encode.Value
encodeStat st =
    Encode.object
        [ ( "date", Encode.string st.dateString )
        , ( "pages_read", Encode.int st.pagesRead )
        ]


followEncoder : List PublicUser -> Encode.Value
followEncoder publicUserList =
    Encode.list Encode.string (List.map .username publicUserList)


encodePublicUser : PublicUser -> Encode.Value
encodePublicUser publicUser =
    Encode.object
        [ ( "username", Encode.string publicUser.username ) ]


userRecordEncoder : User -> Encode.Value
userRecordEncoder user =
    Encode.object
        [ ( "user", userEncoder user ) ]


statusDecoder : Decoder String
statusDecoder =
    Decode.field "message" Decode.string


annotatedUserListDecoder : Decoder (List AnnotatedUser)
annotatedUserListDecoder =
    Decode.field "data" (Decode.list annotatedUserDecoder)


annotatedUserDecoder : Decoder AnnotatedUser
annotatedUserDecoder =
    Decode.succeed AnnotatedUser
        |> required "username" Decode.string
        |> required "id" Decode.int
        |> required "firstname" Decode.string
        |> required "email" Decode.string
        |> required "token" Decode.string
        |> required "blurb" Decode.string
        |> required "public" Decode.bool
        |> required "follow" (Decode.list publicUserDecoder)
        |> required "followers" (Decode.list publicUserDecoder)
        |> required "admin" Decode.bool
        |> required "inserted_at" (Decode.map Utility.usDateStringFromElixirDateString Decode.string)
        |> required "tags" (Decode.list Decode.string)
        |> required "numberOfBooks" Decode.int
        |> required "reading_stats" (Decode.list readingStatDecoder)
        |> required "verified" Decode.bool


readingStatDecoder : Decoder ReadingStat
readingStatDecoder =
    Decode.succeed ReadingStat
        |> required "date" Decode.string
        |> required "pages_read" Decode.int


type alias ReadingStat =
    { dateString : String
    , pagesRead : Int
    }



--
-- ENCODERS AND DECODERS
--


tokenEncoder : String -> Encode.Value
tokenEncoder token =
    Encode.object
        [ ( "token", Encode.string token )
        ]


authorizationEncoder : String -> String -> Encode.Value
authorizationEncoder email password =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]


registrationEncoder : String -> String -> String -> Encode.Value
registrationEncoder username email password =
    Encode.object [ ( "user", preRegistrationEncoder username email password ) ]


preRegistrationEncoder : String -> String -> String -> Encode.Value
preRegistrationEncoder username email password =
    Encode.object
        [ ( "username", Encode.string username )
        , ( "firstname", Encode.string "Anon" )
        , ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]


userRecordDecoder : Decoder UserRecord
userRecordDecoder =
    Decode.succeed UserRecord
        |> required "user" userDecoder


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "username" Decode.string
        |> required "id" Decode.int
        |> required "firstname" Decode.string
        |> required "email" Decode.string
        |> required "token" Decode.string
        |> required "blurb" Decode.string
        |> required "public" Decode.bool
        |> required "follow" (Decode.list publicUserDecoder)
        |> required "followers" (Decode.list publicUserDecoder)
        |> required "admin" Decode.bool
        |> required "inserted_at" (Decode.map Utility.usDateStringFromElixirDateString Decode.string)
        |> required "tags" (Decode.list Decode.string)
        |> required "reading_stats" (Decode.list readingStatDecoder)
        |> required "verified" Decode.bool


userListDecoder : Decoder (List User)
userListDecoder =
    Decode.field "data" (Decode.list userDecoder)


publicUserEncoder : PublicUser -> Encode.Value
publicUserEncoder publicUser =
    Encode.object
        [ ( "username", Encode.string publicUser.username ) ]
