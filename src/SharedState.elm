module SharedState exposing (SharedAppState(..), SharedState, SharedStateUpdate(..), initialSharedState, update)

import Browser.Navigation
import Time exposing (Posix)
import User.Types exposing (User)


type alias SharedState =
    { navKey : Browser.Navigation.Key
    , currentTime : Posix
    , currentUser : Maybe User
    , windowWidth : Int
    , windowHeight : Int
    , appState : SharedAppState
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


initialSharedState : Browser.Navigation.Key -> Posix -> Int -> Int -> Maybe User -> SharedState
initialSharedState navKey time w h currentUser =
    { navKey = navKey
    , currentTime = time
    , currentUser = currentUser
    , windowWidth = w
    , windowHeight = h
    , appState = SharedStateReady
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

        NoUpdate ->
            sharedState
