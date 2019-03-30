module User.Types exposing
    ( Msg(..)
    , State(..)
    , User
    , UserRecord
    , testUser
    )

import Http
import Routing.Helpers exposing (Route)


type alias User =
    { username : String
    , id : Int
    , email : String
    , token : String
    , admin : Bool
    , verified : Bool
    }


type alias UserRecord =
    { user : User }


testUser : User
testUser =
    { username = "fred007"
    , id = 1
    , email = "fred@foo.io"
    , token = "6666555dfdfdf"
    , admin = False
    , verified = True
    }


type Msg
    = ProcessAuthentication (Result Http.Error User)
    | AcceptRegistration (Result Http.Error User)
    | ReceiveUpdateUser (Result Http.Error String)
    | NavigateTo Route
    | AcceptUsername String
    | AcceptEmail String
    | AcceptPassword String
    | SignIn
    | SignOut
    | Register
    | CancelRegistration
    | CancelSignin
    | SubmitRegistration
    | SetState State


type State
    = NotSignedIn
    | SigningIn
    | Registering
    | Registered
    | SignedIn
