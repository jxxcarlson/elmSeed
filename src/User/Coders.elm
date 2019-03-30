module User.Coders exposing
    ( authorizationEncoder
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
import User.Types exposing (User, UserRecord)


userEncoder : User -> Encode.Value
userEncoder user =
    Encode.object
        [ ( "username", Encode.string user.username )
        , ( "id", Encode.int user.id )
        , ( "email", Encode.string user.email )
        , ( "token", Encode.string user.token )
        , ( "admin", Encode.bool user.admin )
        , ( "verified", Encode.bool user.verified )
        ]


userRecordEncoder : User -> Encode.Value
userRecordEncoder user =
    Encode.object
        [ ( "user", userEncoder user ) ]


statusDecoder : Decoder String
statusDecoder =
    Decode.field "message" Decode.string



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
        |> required "email" Decode.string
        |> required "token" Decode.string
        |> required "admin" Decode.bool
        |> required "verified" Decode.bool


userListDecoder : Decoder (List User)
userListDecoder =
    Decode.field "data" (Decode.list userDecoder)


type alias UserRecord =
    { user : User }
