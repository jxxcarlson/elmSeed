module Routing.Router exposing (Model, Msg(..), init, initialModel, pageView, update, updateCurrentUser, view)

import Browser.Navigation exposing (Key)
import Common.Style as Style
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Html exposing (Html)
import Json.Encode
import OutsideInfo exposing (InfoForOutside(..))
import Pages.CurrentUser as CurrentUser
import Routing.Helpers exposing (Route(..), parseUrl, reverseRoute)
import SharedState exposing (SharedAppState(..), SharedState, SharedStateUpdate(..))
import Url exposing (Url)
import User.Types exposing (State(..))
import User.Utility


type alias Model =
    { currentUserModel : CurrentUser.Model
    , route : Route
    }


type Msg
    = UrlChange Url
    | CurrentUserMsg User.Types.Msg
    | NavigateTo Route
    | SignOut


initialModel : Url -> Model
initialModel url =
    let
        currentUserModel =
            CurrentUser.initModel
    in
    { currentUserModel = currentUserModel
    , route = parseUrl url
    }


init : Url -> ( Model, Cmd Msg )
init url =
    let
        currentUserModel =
            CurrentUser.initModel
    in
    ( { currentUserModel = currentUserModel
      , route = parseUrl url
      }
    , Cmd.none
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        UrlChange location ->
            let
                route =
                    parseUrl location

                cmd =
                    case route of
                        CurrentUserRoute ->
                            case ( sharedState.currentUser, sharedState.appState ) of
                                ( _, _ ) ->
                                    Cmd.none

                        _ ->
                            Cmd.none

                oldUserModel =
                    model.currentUserModel

                newUserModel =
                    case route of
                        CurrentUserRoute ->
                            case sharedState.currentUser of
                                Nothing ->
                                    { oldUserModel | state = NotSignedIn }

                                Just _ ->
                                    { oldUserModel | state = SignedIn, tagString = User.Utility.tagsToString sharedState.currentUser }

                        _ ->
                            oldUserModel
            in
            ( { model
                | route = route
                , currentUserModel = newUserModel
              }
            , cmd
            , UpdateSharedAppState SharedStateRunning
            )

        NavigateTo route ->
            ( model
            , Browser.Navigation.pushUrl sharedState.navKey (reverseRoute route)
            , NoUpdate
            )

        CurrentUserMsg currentUserMsg ->
            updateCurrentUser sharedState model currentUserMsg

        SignOut ->
            let
                cmds =
                    [ OutsideInfo.sendInfoOutside (DisconnectUser Json.Encode.null)
                    , Browser.Navigation.pushUrl sharedState.navKey "#/"
                    ]
            in
            ( { model
                | currentUserModel = CurrentUser.initModel
              }
            , Cmd.batch cmds
            , InvalidateCurrentUser
            )


updateCurrentUser : SharedState -> Model -> User.Types.Msg -> ( Model, Cmd Msg, SharedStateUpdate )
updateCurrentUser sharedState model userMsg =
    let
        ( nextUserModel, userCmd, sharedStateUpdate ) =
            CurrentUser.update sharedState userMsg model.currentUserModel
    in
    ( { model | currentUserModel = nextUserModel }
    , Cmd.map CurrentUserMsg userCmd
    , sharedStateUpdate
    )


view : (Msg -> msg) -> SharedState -> Model -> { body : List (Html.Html msg), title : String }
view msgMapper sharedState model =
    mainView msgMapper sharedState model


mainView : (Msg -> msg) -> SharedState -> Model -> { body : List (Html.Html msg), title : String }
mainView msgMapper sharedState model =
    let
        title =
            case model.route of
                CurrentUserRoute ->
                    "Current User"

                NotFoundRoute ->
                    "404"

        body_ =
            column [ paddingXY 0 0, Background.color Style.grey, width fill, height fill ]
                [ row
                    (Style.navBar fill)
                    [ Input.button (Style.activeButton (model.route == CurrentUserRoute))
                        { onPress = Just (NavigateTo CurrentUserRoute)
                        , label = el [] (text "User")
                        }
                    , showIf (sharedState.currentUser /= Nothing)
                        (Input.button (Style.button ++ [ alignRight ])
                            { onPress = Just SignOut
                            , label = el [] (text "Sign out")
                            }
                        )
                    ]
                , pageView sharedState model
                ]
    in
    { title = "BookLib"
    , body = body_ |> Element.layoutWith { options = [ Style.myFocusStyle ] } [] |> Html.map msgMapper |> (\x -> [ x ])
    }


pageView : SharedState -> Model -> Element Msg
pageView sharedState model =
    row []
        [ case model.route of
            CurrentUserRoute ->
                CurrentUser.view sharedState model.currentUserModel
                    |> Element.map CurrentUserMsg

            NotFoundRoute ->
                el [] (text "404 :(")
        ]



--
-- HELPERS
--


showIf : Bool -> Element Msg -> Element Msg
showIf flag element =
    if flag then
        element

    else
        Element.none


currentUserIsMe : SharedState -> Bool
currentUserIsMe sharedState =
    case sharedState.currentUser of
        Nothing ->
            False

        Just user ->
            user.username == "jxxcarlson"
