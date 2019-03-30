module User.Session exposing (authenticate, registerUser, updateUser)

import Common.Utility as Utility
import Configuration
import Http
import User.Coders
import User.Types exposing (Msg(..), PublicUser, ReadingStat, User, UserRecord)


authenticate : String -> String -> Cmd Msg
authenticate email password =
    Http.post
        { url = Configuration.backend ++ "/api/users/authenticate"
        , body = Http.jsonBody (User.Coders.authorizationEncoder email password)
        , expect = Http.expectJson ProcessAuthentication User.Coders.userDecoder
        }


registerUser : String -> String -> String -> Cmd Msg
registerUser username email password =
    Http.post
        { url = Configuration.backend ++ "/api/users/"
        , body = Http.jsonBody (User.Coders.registrationEncoder username email password)
        , expect = Http.expectJson AcceptRegistration User.Coders.userDecoder
        }


updateUser : User -> String -> Cmd Msg
updateUser user token =
    Http.request
        { method = "Put"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Configuration.backend ++ "/api/users/" ++ String.fromInt user.id
        , body = Http.jsonBody (User.Coders.userRecordEncoder user)
        , expect = Http.expectJson ReceiveUpdateUser User.Coders.statusDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
