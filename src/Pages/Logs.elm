module Pages.Logs exposing
    ( Model
    , Msg(..)
    , initModel
    , update
    , view
    )

import Common.Style as Style
import Configuration
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Logger.Object
import Logger.Object.Event
import Logger.Query as Query
import SharedState exposing (Event, Log, SharedState, SharedStateUpdate(..))


type alias Model =
    { message : String }


initModel : Model
initModel =
    { message = "Nothing yet." }


type Msg
    = NoOp
    | GetEvents Int
    | GotEvents (Result (Graphql.Http.Error EventListResponse) EventListResponse)


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoUpdate )

        GetEvents logId ->
            ( model, getEvents logId, NoUpdate )

        GotEvents (Ok events) ->
            ( { model | message = "Events received" }, Cmd.none, UpdateSharedEventList events )

        GotEvents (Err _) ->
            ( { model | message = "Error getting events" }, Cmd.none, NoUpdate )


view : SharedState -> Model -> Element Msg
view sharedState model =
    column (Style.mainColumn fill fill ++ [ padding 40, Background.color (Style.makeGrey 0.8) ])
        [ row [ spacing 20 ]
            [ el [] (text "LOGS!!")
            , testButton
            ]
        ]


testButton : Element Msg
testButton =
    Input.button Style.button
        { onPress = Just (GetEvents 1)
        , label = Element.text "Test"
        }



--
-- GRAPHQL
--


query : Int -> SelectionSet (List Event) RootQuery
query logId =
    Query.listEventsForLog { logId = logId } eventSelection


eventSelection : SelectionSet Event Logger.Object.Event
eventSelection =
    SelectionSet.map2 Event
        Logger.Object.Event.id
        Logger.Object.Event.value


type alias EventListResponse =
    List Event


getEvents : Int -> Cmd Msg
getEvents logId =
    query logId
        |> Graphql.Http.queryRequest (Configuration.backend ++ "/graphiql")
        -- |> Graphql.Http.withHeader "authorization" "Bearer <your github bearer token>"
        |> Graphql.Http.send GotEvents


query1 : SelectionSet () RootQuery
query1 =
    SelectionSet.empty
