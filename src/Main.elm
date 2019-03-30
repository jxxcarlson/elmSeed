module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation exposing (pushUrl)
import Configuration
import Html exposing (..)
import Json.Encode
import OutsideInfo exposing (InfoForElm(..), InfoForOutside(..))
import Routing.Router as Router
import SharedState exposing (SharedState, SharedStateUpdate(..), initialSharedState)
import Time exposing (Posix)
import Url exposing (Url)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , onUrlChange = UrlChange
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        }


subscriptions model =
    Sub.batch
        [ OutsideInfo.getInfoFromOutside Outside LogErr
        , case Configuration.site == "LOCAL" of
            True ->
                Time.every 100000 TimeChange

            False ->
                Time.every 1000 TimeChange
        ]


type alias Model =
    { appState : AppState
    , navKey : Browser.Navigation.Key
    , url : Url
    , windowWidth : Int
    , windowHeight : Int
    }


type alias Flags =
    { currentTime : Int, width : Int, height : Int }


type AppState
    = NotReady Url Posix Browser.Navigation.Key
    | Ready SharedState Router.Model
    | FailedToInitialize


type Msg
    = UrlChange Url
    | LinkClicked UrlRequest
    | TimeChange Posix
    | RouterMsg Router.Msg
    | Outside InfoForElm
    | LogErr String


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    ( { appState =
            -- Ready
            --     (initialSharedState navKey (Time.millisToPosix flags.currentTime))
            --     (Router.initialModel url)
            NotReady url (Time.millisToPosix flags.currentTime) navKey
      , url = url
      , navKey = navKey
      , windowWidth = flags.width
      , windowHeight = flags.height
      }
    , Cmd.batch
        [ OutsideInfo.sendInfoOutside (AskToReconnectUser Json.Encode.null)
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeChange time ->
            updateTime model time

        UrlChange url ->
            updateRouter { model | url = url } (Router.UrlChange url)

        RouterMsg routerMsg ->
            updateRouter model routerMsg

        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Browser.Navigation.pushUrl model.navKey (Url.toString url) )

                External url ->
                    ( model, Browser.Navigation.load url )

        Outside infoForElm ->
            case model.appState of
                NotReady url posix navKey ->
                    reconnectUser model url posix navKey infoForElm

                _ ->
                    ( model, Cmd.none )

        LogErr _ ->
            case model.appState of
                NotReady url posix navKey ->
                    ( { model
                        | appState =
                            Ready
                                (initialSharedState navKey posix model.windowWidth model.windowHeight Nothing)
                                (Router.initialModel url)
                        , url = url
                        , navKey = navKey
                      }
                    , OutsideInfo.sendInfoOutside (AskToReconnectUser Json.Encode.null)
                    )

                _ ->
                    ( model, Cmd.none )


updateTime : Model -> Posix -> ( Model, Cmd Msg )
updateTime model time =
    case model.appState of
        NotReady url _ navKey ->
            ( { model | appState = NotReady url time navKey }
            , Cmd.none
            )

        Ready sharedState routerModel ->
            ( { model | appState = Ready (SharedState.update sharedState (UpdateTime time)) routerModel }
            , Cmd.none
            )

        FailedToInitialize ->
            ( model, Cmd.none )


updateRouter : Model -> Router.Msg -> ( Model, Cmd Msg )
updateRouter model routerMsg =
    case model.appState of
        Ready sharedState routerModel ->
            let
                nextSharedState =
                    SharedState.update sharedState sharedStateUpdate

                ( nextRouterModel, routerCmd, sharedStateUpdate ) =
                    Router.update sharedState routerMsg routerModel
            in
            ( { model | appState = Ready nextSharedState nextRouterModel }
            , Cmd.map RouterMsg routerCmd
            )

        _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    case model.appState of
        Ready sharedState routerModel ->
            Router.view RouterMsg sharedState routerModel

        NotReady _ _ _ ->
            { title = "Loading (1)"
            , body = [ text "Loading (2)" ]
            }

        FailedToInitialize ->
            { title = "Failure"
            , body = [ text "The application failed to initialize. " ]
            }



--
-- LOCAL STORAGE
--


reconnectUser : Model -> Url -> Posix -> Browser.Navigation.Key -> InfoForElm -> ( Model, Cmd Msg )
reconnectUser model url posix navKey (LocalStorageInfo user) =
    let
        beginningDate =
            user.beginningDate

        appState =
            Ready
                (initialSharedState navKey posix model.windowWidth model.windowHeight (Just user))
                (Router.initialModel url)
    in
    ( { model | appState = appState }, Cmd.batch [ pushUrl navKey "#books" ] )
