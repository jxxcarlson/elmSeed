module Pages.CurrentUser exposing
    ( Model
    , currentSharedStateView
    , initModel
    , update
    , view
    )

import Browser.Navigation exposing (pushUrl)
import Common.Style as Style
import Common.Utility as Utility
import Configuration
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http exposing (Error(..))
import Json.Encode
import OutsideInfo exposing (InfoForOutside(..))
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import User.Session as Session
import User.Types exposing (Msg(..), ReadingStat, State(..), User)


type alias Model =
    { message : String
    , email : String
    , password : String
    , username : String
    , tagString : String
    , state : State
    , errorResponse : ErrorResponse
    }


initModel : Model
initModel =
    { message = ""
    , username = ""
    , email = ""
    , password = ""
    , tagString = ""
    , state = NotSignedIn
    , errorResponse = NoErrorResponse
    }


{-| NOTE that the Settings updpate function takes a SharedState parameter.
In addition, its return value is a triple, with the last element
of type SharedStateUpdate.
-}
update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        AcceptEmail str ->
            ( { model | email = str }, Cmd.none, NoUpdate )

        AcceptPassword str ->
            ( { model | password = str }, Cmd.none, NoUpdate )

        AcceptUsername str ->
            ( { model | username = str }, Cmd.none, NoUpdate )

        SignIn ->
            ( { model | message = "" }, Session.authenticate model.email model.password, NoUpdate )

        SignOut ->
            ( { model
                | message = ""
                , username = ""
                , email = ""
                , password = ""
                , state = NotSignedIn
                , errorResponse = NoErrorResponse
              }
            , OutsideInfo.sendInfoOutside (DisconnectUser Json.Encode.null)
            , InvalidateCurrentUser
            )

        Register ->
            ( { model | message = "", state = Registering }, Cmd.none, NoUpdate )

        CancelRegistration ->
            ( { model | message = "", state = NotSignedIn }, Cmd.none, NoUpdate )

        CancelSignin ->
            ( { model | message = "", state = NotSignedIn, errorResponse = NoErrorResponse }, Cmd.none, NoUpdate )

        SubmitRegistration ->
            ( { model | message = "", state = Registering }, Session.registerUser model.username model.email model.password, NoUpdate )

        ProcessAuthentication (Ok user) ->
            ( { model
                | message = "Welcome " ++ user.username ++ "."
                , username = ""
                , email = ""
                , password = ""
                , state = SignedIn
              }
            , Cmd.batch
                [ OutsideInfo.sendInfoOutside (UserData (OutsideInfo.userEncoder user))
                ]
            , UpdateCurrentUser (Just user)
            )

        ProcessAuthentication (Err err) ->
            ( { model
                | message = "Invalid password or username"
                , state = SigningIn
                , errorResponse = ShowPasswordReset
              }
            , Cmd.none
            , UpdateCurrentUser Nothing
            )

        AcceptRegistration (Ok user) ->
            ( { model
                | message = "Welcome to Booklib.io, " ++ user.username ++ "!"
                , username = ""
                , email = ""
                , password = ""
                , state = Registered
              }
            , Cmd.none
            , UpdateCurrentUser (Just user)
            )

        AcceptRegistration (Err err) ->
            ( { model | message = httpErrorExplanation err, state = SigningIn }, Cmd.none, UpdateCurrentUser Nothing )

        ReceiveUpdateUser (Ok user) ->
            ( { model | message = "Your profile has been updated" }, Cmd.none, NoUpdate )

        ReceiveUpdateUser (Err err) ->
            ( { model | message = "Error updating user" }, Cmd.none, NoUpdate )

        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        SetState state ->
            ( { model | state = state }, Cmd.none, NoUpdate )

        ToggleUserPublic flag ->
            case sharedState.currentUser of
                Nothing ->
                    ( model, Cmd.none, NoUpdate )

                Just user ->
                    let
                        nextUser =
                            { user | public = flag }

                        cmd =
                            Session.updateUser nextUser nextUser.token
                    in
                    ( model, cmd, SharedState.UpdateCurrentUser (Just nextUser) )

        InputTagString str ->
            ( { model | tagString = str }, Cmd.none, NoUpdate )

        UpdateUserTags ->
            let
                ( nextCurrentUser, cmd ) =
                    case sharedState.currentUser of
                        Nothing ->
                            ( Nothing, Cmd.none )

                        Just user ->
                            let
                                nextUser_ =
                                    { user | tags = String.split "," model.tagString |> List.map String.trim }
                            in
                            ( Just nextUser_, Session.updateUser nextUser_ nextUser_.token )
            in
            ( model, cmd, SharedState.UpdateCurrentUser nextCurrentUser )


view : SharedState -> Model -> Element Msg
view sharedState model =
    case classifyDevice { width = sharedState.windowWidth, height = sharedState.windowHeight } |> .class of
        Phone ->
            column (Style.mainColumn fill fill)
                [ showIf (model.state /= SignedIn) (row [] [ el [ Font.bold ] (text "Welcome to Booklib.io") ])
                , showIf (model.state /= SignedIn) (row [ Font.size 14 ] [ el [] (text "Create and share your annotated book list") ])
                , signInColumn sharedState model
                , image [ width fill ]
                    { src = "https://www.hastac.org/sites/default/files/upload/images/post/books.jpg"
                    , description = "Library"
                    }
                , infoLine
                , footer sharedState model
                ]

        _ ->
            column (Style.mainColumn fill fill)
                [ row [ spacing 20 ]
                    [ welcomeColumn sharedState model
                    , signInColumn sharedState model
                    ]
                , footer sharedState model
                ]


infoLine =
    el [ Font.size 14 ] (text "Bug reports and suggestions to jxxcarlson@gmail.com")


welcomeColumn : SharedState -> Model -> Element Msg
welcomeColumn sharedState model =
    column [ alignTop, spacing 10 ]
        [ showIf (model.state /= SignedIn) (row [] [ el [ Font.bold ] (text "Welcome to Booklib.io") ])
        , showIf (model.state /= SignedIn) (row [] [ el [] (text "Create and share your annotated book list") ])
        , image []
            { src = "https://www.hastac.org/sites/default/files/upload/images/post/books.jpg"
            , description = "Library"
            }
        ]



-- https://www.hastac.org/sites/default/files/upload/images/post/books.jpg


signInColumn : SharedState -> Model -> Element Msg
signInColumn sharedState model =
    column (Style.signinColumn fill fill)
        [ showIf (not <| showIfSignedInOrRegistered model) (inputUsername sharedState model)
        , showIf (not <| showIfSignedInOrRegistered model) (inputEmail sharedState model)
        , showIf (not <| showIfSignedInOrRegistered model) (inputPassword sharedState model)
        , showIf (not <| showIfSignedInOrRegistered model) (row (registrationStyle sharedState) [ signInOrCancelButton sharedState model, registerButton model ])
        , showIf (showIfSignedInOrRegistered model) (signInOrCancelButton sharedState model)
        , messagePanel model
        , showIf (showIfSignedInOrRegistered model) (publicCheckbox sharedState)
        , tagSection sharedState model
        , showIf (not <| showIfSignedInOrRegistered model) (resetPasswordLink model)
        , verifyUserLink model
        , infoPanel sharedState model
        , tipLine
        ]


tagSection sharedState model =
    showIf (showIfSignedInOrRegistered model)
        (column [ padding 12, spacing 8, Border.width 1 ]
            [ tagInput sharedState model
            , tagUpdateButton
            ]
        )


tipLine =
    column [ Font.size 14, spacing 8 ]
        [ el [ Font.bold ] (text "Tip")
        , paragraph
            [ width (px 350) ]
            [ text "To add new book after you are signed in, click on the 'Book' tab, then 'New' in the footer." ]
        ]


messagePanel model =
    case model.state of
        Registered ->
            column [ Font.size 16, paddingXY 0 24, spacing 8 ]
                [ el [] (text model.message)
                , el [] (text <| "We've sent you an email with a verification link.")
                , el [] (text <| "To add a new book, click on the BOOK tab,")
                , el [] (text <| "then choose NEW.")
                ]

        _ ->
            el [ Font.size 16 ] (text model.message)


showIfSignedInOrRegistered : Model -> Bool
showIfSignedInOrRegistered model =
    List.member model.state [ SignedIn, Registered ]


infoPanel sharedState model =
    case deviceIsPhone sharedState of
        True ->
            Element.none

        False ->
            showIf (model.state == SignedIn) <|
                column [ paddingXY 0 18, spacing 12 ]
                    [ infoLine
                    ]


registrationStyle sharedState =
    case deviceIsPhone sharedState of
        True ->
            [ moveRight 65, spacing 12 ]

        False ->
            [ moveRight 125, spacing 12 ]


tagInput sharedState model =
    case classifyDevice { width = sharedState.windowWidth, height = sharedState.windowHeight } |> .class of
        Phone ->
            Input.text [ width (px 280), height (px 20), Font.size 14 ]
                { text = model.tagString
                , placeholder = Just <| Input.placeholder [ moveUp 6 ] (el [] (Element.text "history, ficton, science"))
                , onChange = InputTagString
                , label = Input.labelLeft [ Font.size 14, moveDown 5 ] (text "Tags ")
                }

        _ ->
            Input.text [ width (px 400), height (px 20), Font.size 14 ]
                { text = model.tagString
                , placeholder = Just <| Input.placeholder [ moveUp 6 ] (el [] (Element.text "history, ficton, science"))
                , onChange = InputTagString
                , label = Input.labelLeft [ Font.size 14, moveDown 5 ] (text "Tags ")
                }


tagUpdateButton =
    Input.button Style.smallButton
        { onPress = Just UpdateUserTags
        , label = el [ Font.size 12 ] (text "Update tags ")
        }


footer : SharedState -> Model -> Element Msg
footer sharedState model =
    row Style.footer
        [ el Style.footerItem (text <| "UTC: " ++ Utility.toUtcString (Just sharedState.currentTime))
        , el Style.footerItem (text <| stateAsString sharedState model.state)
        ]


deviceIsPhone : SharedState -> Bool
deviceIsPhone sharedState =
    case classifyDevice { width = sharedState.windowWidth, height = sharedState.windowHeight } |> .class of
        Phone ->
            True

        _ ->
            False


showIf : Bool -> Element Msg -> Element Msg
showIf flag element =
    if flag then
        element

    else
        Element.none


inputEmail sharedState model =
    Input.text inputStyle
        { onChange = AcceptEmail
        , text = model.email
        , placeholder = Nothing
        , label = inputLabel sharedState (inputLabelStyle sharedState) (text "Email")
        }


publicCheckbox : SharedState -> Element Msg
publicCheckbox sharedState =
    case sharedState.currentUser of
        Nothing ->
            Element.none

        Just user ->
            Input.checkbox
                []
                { onChange = ToggleUserPublic
                , icon = icon
                , checked = user.public
                , label = Input.labelLeft [ Font.size 16, moveDown 1 ] (text "Share your reading list?")
                }


icon : Bool -> Element msg
icon status =
    case status of
        True ->
            el [ Font.size 18, Font.bold, Font.color Style.blue, moveDown 1 ] (text "Yes")

        False ->
            el [ Font.size 18, Font.bold, Font.color Style.darkRed, moveDown 1 ] (text "No")


inputStyle =
    [ width (px 300), Background.color (Style.makeGrey 0.3), Font.color Style.white, Style.noAutocapitalize ]


inputLabelStyle sharedState =
    case deviceIsPhone sharedState of
        True ->
            [ Font.bold, width (px 120) ]

        False ->
            [ moveDown 12, Font.bold, width (px 120) ]


inputUsername sharedState model =
    case model.state of
        Registering ->
            Input.text inputStyle
                { onChange = AcceptUsername
                , text = model.username
                , placeholder = Nothing
                , label = inputLabel sharedState (inputLabelStyle sharedState) (text "Username")
                }

        _ ->
            Element.none


inputPassword sharedState model =
    Input.currentPassword inputStyle
        { onChange = AcceptPassword
        , text = model.password
        , placeholder = Nothing
        , label = inputLabel sharedState (inputLabelStyle sharedState) (text "Password")
        , show = False
        }


inputLabel sharedState =
    case deviceIsPhone sharedState of
        True ->
            Input.labelAbove

        False ->
            Input.labelLeft


signInOrCancelButton sharedState model =
    case model.state of
        NotSignedIn ->
            signInButton

        SigningIn ->
            signInButton

        Registering ->
            cancelRegistrationButton

        Registered ->
            signOutButton

        SignedIn ->
            case deviceIsPhone sharedState of
                True ->
                    signOutButton

                False ->
                    Element.none



-- signOutButton


signInButton =
    Input.button Style.button
        { onPress = Just SignIn
        , label = el [] (text "Sign In")
        }


signOutButton =
    Input.button Style.button
        { onPress = Just SignOut
        , label = el [] (text "Sign Out")
        }


cancelRegistrationButton =
    Input.button Style.button
        { onPress = Just CancelRegistration
        , label = el [] (text "Cancel")
        }


cancelSignInButton =
    Input.button Style.button
        { onPress = Just CancelSignin
        , label = el [] (text "Cancel")
        }


registerButton model =
    case model.state of
        NotSignedIn ->
            registerButton_

        SigningIn ->
            cancelSignInButton

        Registering ->
            submitRegistrationButton

        Registered ->
            Element.none

        SignedIn ->
            Element.none


registerButton_ =
    Input.button Style.button
        { onPress = Just Register
        , label = el [] (text "Register")
        }


submitRegistrationButton =
    Input.button Style.button
        { onPress = Just SubmitRegistration
        , label = el [] (text "Submit registration")
        }


currentSharedStateView : SharedState -> Element never
currentSharedStateView sharedState =
    column [ paddingXY 0 20, spacing 24 ]
        [ el [ Font.size 16 ] (text <| "UTC: " ++ Utility.toUtcString (Just sharedState.currentTime))
        ]



--
-- HELPERS
--


stateAsString : SharedState -> State -> String
stateAsString sharedState state =
    case state of
        NotSignedIn ->
            "Not signed in"

        SignedIn ->
            "Signed in as " ++ userStatus sharedState.currentUser

        SigningIn ->
            "Signing in"

        Registered ->
            "Registered"

        Registering ->
            "Registering"


userStatus : Maybe User -> String
userStatus user_ =
    case user_ of
        Nothing ->
            ""

        Just user ->
            user.username


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int
    | BadBody String


httpErrorExplanation : Http.Error -> String
httpErrorExplanation error =
    case error of
        Http.BadUrl str ->
            str

        Http.Timeout ->
            "Timeout (error)"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody str ->
            cleanupErrorMessage str


cleanupErrorMessage : String -> String
cleanupErrorMessage str =
    str
        |> String.lines
        |> List.filter (\x -> String.contains "message" x)
        |> List.head
        |> Maybe.withDefault ""
        |> String.trim
        |> String.replace "message" ""
        |> String.replace "\"" ""
        |> String.dropLeft 1



--
-- REQUESTS
--
--
-- PASSWORD RESET
--


type ErrorResponse
    = ShowPasswordReset
    | ShowVerifyAccount
    | NoErrorResponse


resetPasswordLink : Model -> Element Msg
resetPasswordLink model =
    case model.errorResponse of
        ShowPasswordReset ->
            newTabLink [ Font.color Style.blue ]
                { url = Configuration.backend ++ "/api/password/request"
                , label = text "Reset password?"
                }

        _ ->
            Element.none


verifyUserLink : Model -> Element Msg
verifyUserLink model =
    case model.errorResponse of
        ShowVerifyAccount ->
            newTabLink []
                { url = Configuration.backend ++ "/api/request_verification"
                , label = text "Request verification?"
                }

        _ ->
            Element.none
