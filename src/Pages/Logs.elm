module Pages.Logs exposing (Model, Msg(..), initModel, update, view)

import Common.Style as Style
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import SharedState exposing (SharedState, SharedStateUpdate(..))


type alias Model =
    { message : String }


initModel : Model
initModel =
    { message = "Nothing yet." }


type Msg
    = NoOp


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> Element Msg
view sharedState model =
    column (Style.mainColumn fill fill)
        [ row [ spacing 20 ]
            [ el [] (text "LOGS!!")
            ]
        ]
