module User.Types exposing
    ( AnnotatedUser
    , Msg(..)
    , PublicUser
    , ReadingStat
    , State(..)
    , User
    , UserRecord
    , testUser
    , userFromAnnotatedUser
    )

import Http
import Routing.Helpers exposing (Route)


type alias User =
    { username : String
    , id : Int
    , firstname : String
    , email : String
    , token : String
    , blurb : String
    , public : Bool
    , follow : List PublicUser
    , followers : List PublicUser
    , admin : Bool
    , beginningDate : String
    , tags : List String
    , readingStats : List ReadingStat
    , verified : Bool
    }


type alias ReadingStat =
    { dateString : String
    , pagesRead : Int
    }


type alias AnnotatedUser =
    { username : String
    , id : Int
    , firstname : String
    , email : String
    , token : String
    , blurb : String
    , public : Bool
    , follow : List PublicUser
    , followers : List PublicUser
    , admin : Bool
    , beginningDate : String
    , tags : List String
    , numberOfBooks : Int
    , readingStats : List ReadingStat
    , verified : Bool
    }


userFromAnnotatedUser : AnnotatedUser -> User
userFromAnnotatedUser u =
    { username = u.username
    , id = u.id
    , firstname = u.firstname
    , email = u.email
    , token = u.token
    , blurb = u.blurb
    , public = u.public
    , follow = u.follow
    , followers = u.followers
    , admin = u.admin
    , beginningDate = u.beginningDate
    , tags = u.tags
    , readingStats = u.readingStats
    , verified = u.verified
    }


type alias UserRecord =
    { user : User }


testUser : User
testUser =
    { username = "fred007"
    , id = 1001
    , firstname = "Fred"
    , email = "fred@foo.io"
    , token = "6666555dfdfdf"
    , blurb = "Howdy!"
    , public = False
    , follow = [ { username = "Abe" } ]
    , followers = [ { username = "Bo" }, { username = "Sally" } ]
    , admin = False
    , beginningDate = "1/1/1800"
    , tags = []
    , readingStats =
        [ { dateString = "2019-01-31", pagesRead = 400 }
        , { dateString = "2019-02-28", pagesRead = 800 }
        , { dateString = "2019-03-31", pagesRead = 1000 }
        , { dateString = "2019-04-30", pagesRead = 1100 }
        , { dateString = "2019-05-31", pagesRead = 1600 }
        ]
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
    | ToggleUserPublic Bool
    | InputTagString String
    | UpdateUserTags


type State
    = NotSignedIn
    | SigningIn
    | Registering
    | Registered
    | SignedIn


type alias PublicUser =
    { username : String }
