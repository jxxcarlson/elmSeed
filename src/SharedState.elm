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
import Logger.Scalar exposing (NaiveDateTime(..))
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
    , currentLogList : Maybe (List Log)
    , currentEventList : Maybe (List Event)
    }


type alias Log =
    { id : Int
    , name : String
    , userId : Int

    --, logType : String
    -- , events : List Event
    }


type alias Event =
    { id : Int
    , value : String
    , insertedAt : NaiveDateTime
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
    | UpdateSharedLogList (List Log)
    | UpdateCurrentLog (Maybe Log)
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
    , currentLogList = Nothing
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

        UpdateSharedLogList logs ->
            { sharedState | currentLogList = Just logs }

        UpdateSharedEventList eventList ->
            { sharedState | currentEventList = Just eventList }

        UpdateCurrentLog maybeLog ->
            { sharedState | currentLog = maybeLog }

        NoUpdate ->
            sharedState
