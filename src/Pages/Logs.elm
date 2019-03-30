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
import Logger.Scalar exposing (NaiveDateTime(..))
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
        [ logPanel sharedState model
        ]


logPanel : SharedState -> Model -> Element Msg
logPanel sharedState model =
    column [ spacing 20, height (px 490), width (px 300) ]
        [ el [] (text "LOGS")
        , viewLog sharedState model
        , testButton
        ]


viewLog : SharedState -> Model -> Element Msg
viewLog sharedState model =
    case sharedState.currentEventList of
        Nothing ->
            column [ height (px 400) ] [ text "No logs available" ]

        Just events ->
            indexedTable [ spacing 4, Font.size 12 ]
                { data = events
                , columns =
                    [ { header = el [ Font.bold ] (text "k")
                      , width = px 40
                      , view = \k event -> el [ Font.size 12 ] (text <| String.fromInt <| k + 1)
                      }
                    , { header = el [ Font.bold ] (text "Date")
                      , width = px 80
                      , view = \k event -> el [ Font.size 12 ] (text <| dateStringOfNaiveDateTime <| event.insertedAt)
                      }
                    , { header = el [ Font.bold ] (text "Time")
                      , width = px 80
                      , view = \k event -> el [ Font.size 12 ] (text <| timeStringOfNaiveDateTime <| event.insertedAt)
                      }
                    , { header = el [ Font.bold ] (text "value")
                      , width = px 40
                      , view = \k event -> el [ Font.size 12 ] (text <| event.value)
                      }
                    ]
                }


stringValueOfNaiveDateTime : NaiveDateTime -> String
stringValueOfNaiveDateTime (NaiveDateTime str) =
    str


dateStringOfNaiveDateTime : NaiveDateTime -> String
dateStringOfNaiveDateTime (NaiveDateTime str) =
    str
        |> String.split "T"
        |> List.head
        |> Maybe.withDefault "-"


timeStringOfNaiveDateTime : NaiveDateTime -> String
timeStringOfNaiveDateTime (NaiveDateTime str) =
    str
        |> String.split "T"
        |> List.reverse
        |> List.head
        |> Maybe.withDefault "-"


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
    SelectionSet.map3 Event
        Logger.Object.Event.id
        Logger.Object.Event.value
        Logger.Object.Event.insertedAt


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
