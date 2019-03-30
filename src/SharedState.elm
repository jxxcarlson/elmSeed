module SharedState exposing
    ( Event
    , Log
    , SharedAppState(..)
    , SharedState
    , SharedStateUpdate(..)
    , initialSharedState
    , update
    )

import Browser.Navigation
import Logger.Enum.LogTypeValue exposing (LogTypeValue(..))
import Time exposing (Posix)
import User.Types exposing (User, testUser)


type alias SharedState =
    { navKey : Browser.Navigation.Key
    , currentTime : Posix
    , windowWidth : Int
    , windowHeight : Int
    , appState : SharedAppState
    , currentUser : Maybe User
    , currentLog : Maybe Log
    , currentEventList : Maybe (List Event)
    }


type alias Log =
    { id : Int
    , name : String
    , userId : Int
    , logType : LogTypeValue
    , events : List Event
    }


type alias Event =
    { id : Int
    , value : String
    }


type SharedAppState
    = SharedStateReady
    | SharedStateStarting
    | SharedStateRunning


type SharedStateUpdate
    = NoUpdate
    | UpdateTime Posix
    | UpdateCurrentUser (Maybe User)
    | InvalidateCurrentUser
    | UpdateSharedAppState SharedAppState
    | UpdateSharedEventList (List Event)


initialSharedState : Browser.Navigation.Key -> Posix -> Int -> Int -> Maybe User -> SharedState
initialSharedState navKey time w h currentUser =
    { navKey = navKey
    , currentTime = time
    , windowWidth = w
    , windowHeight = h
    , appState = SharedStateReady
    , currentUser = Just testUser
    , currentLog = Nothing
    , currentEventList = Nothing
    }


update : SharedState -> SharedStateUpdate -> SharedState
update sharedState sharedStateUpdate =
    case sharedStateUpdate of
        UpdateTime time ->
            { sharedState | currentTime = time }

        UpdateCurrentUser currentUser ->
            { sharedState | currentUser = currentUser, appState = SharedStateStarting }

        InvalidateCurrentUser ->
            { sharedState | currentUser = Nothing, appState = SharedStateReady }

        UpdateSharedAppState appState ->
            { sharedState | appState = appState }

        UpdateSharedEventList eventList ->
            { sharedState | currentEventList = Just eventList }

        NoUpdate ->
            sharedState
