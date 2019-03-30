module Pages.Logs exposing
    ( Model
    , Msg(..)
    , initModel
    , update
    , view
    )

import Common.Style as Style
import Common.Utility as Utility
import Configuration
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Json.Decode as Decode exposing (Decoder)
import Logger.Enum.LogTypeValue as LogTypeValue exposing (LogTypeValue(..))
import Logger.Mutation as Mutation exposing (CreateEventRequiredArguments, CreateLogRequiredArguments, createEvent, createLog)
import Logger.Object
import Logger.Object.Event
import Logger.Object.Log
import Logger.Query as Query
import Logger.Scalar exposing (NaiveDateTime(..))
import SharedState exposing (Event, Log, SharedState, SharedStateUpdate(..))


type alias Model =
    { message : String
    , valueString : String
    }


initModel : Model
initModel =
    { message = "Nothing yet."
    , valueString = ""
    }



--
-- MSG
--


type Msg
    = NoOp
    | GetLogs Int
    | GotLogs (Result (Graphql.Http.Error LogListResponse) LogListResponse)
    | GetEvents Int
    | GotEvents (Result (Graphql.Http.Error EventListResponse) EventListResponse)
    | EventCreated (Result (Graphql.Http.Error (Maybe Event)) (Maybe Event))
    | MakeEvent
    | GotValueString String


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoUpdate )

        GetLogs userId ->
            ( model, getLogs userId, NoUpdate )

        GotLogs (Ok logs) ->
            ( { model | message = "Logs received" }, Cmd.none, UpdateSharedLogList logs )

        GotLogs (Err _) ->
            ( { model | message = "Error getting logs" }, Cmd.none, NoUpdate )

        GetEvents logId ->
            let
                maybeLog =
                    Maybe.map (List.filter (\log -> log.id == logId)) sharedState.currentLogList
                        |> Maybe.andThen List.head
            in
            ( model, getEvents logId, UpdateCurrentLog maybeLog )

        EventCreated (Ok maybeEvent) ->
            case ( maybeEvent, sharedState.currentEventList ) of
                ( Nothing, _ ) ->
                    ( { model | message = "Event could not be created" }, Cmd.none, NoUpdate )

                ( Just event, Just eventList ) ->
                    ( { model | message = "Event  created" }, Cmd.none, UpdateSharedEventList (event :: eventList) )

                ( _, _ ) ->
                    ( { model | message = "Oops!" }, Cmd.none, NoUpdate )

        EventCreated (Err _) ->
            ( { model | message = "Error crearing event" }, Cmd.none, NoUpdate )

        GotEvents (Ok events) ->
            ( { model | message = "Events received" }, Cmd.none, UpdateSharedEventList events )

        GotEvents (Err _) ->
            ( { model | message = "Error getting events" }, Cmd.none, NoUpdate )

        GotValueString str ->
            ( { model | valueString = str }, Cmd.none, NoUpdate )

        MakeEvent ->
            case sharedState.currentLog of
                Nothing ->
                    ( { model | message = "No log available to make event" }, Cmd.none, NoUpdate )

                Just log ->
                    ( { model | message = "Making new event for log " ++ String.fromInt log.id }
                    , makeEvent log.id model.valueString
                    , NoUpdate
                    )


view : SharedState -> Model -> Element Msg
view sharedState model =
    column (Style.mainColumn fill fill ++ [ spacing 12, padding 40, Background.color (Style.makeGrey 0.8) ])
        [ row [ spacing 12 ]
            [ logListPanel sharedState model
            , eventsPanel sharedState model
            ]
        , row [ spacing 12, alignBottom ] [ getLogsButton, inputValue model, submitEventButton ]
        ]


logListPanel : SharedState -> Model -> Element Msg
logListPanel sharedState model =
    column [ spacing 20, height (px 490), width (px 300), Border.width 1 ]
        [ viewLogs sharedState model
        ]


viewLogs : SharedState -> Model -> Element Msg
viewLogs sharedState model =
    case sharedState.currentLogList of
        Nothing ->
            column [ spacing 12, padding 20, height (px 400), Font.size 16 ]
                [ el [ Font.bold ] (text "No logs available")
                ]

        Just events ->
            column [ spacing 12, padding 20, height (px 400) ]
                [ el [ Font.size 16, Font.bold ] (text "Logs")
                , indexedTable
                    [ spacing 4, Font.size 12 ]
                    { data = events
                    , columns =
                        [ { header = el [ Font.bold ] (text "k")
                          , width = px 40
                          , view = \k log -> el [ Font.size 12 ] (text <| String.fromInt <| k + 1)
                          }
                        , { header = el [ Font.bold ] (text "Name")
                          , width = px 80
                          , view = \k log -> el [ Font.size 12 ] (logNameButton sharedState.currentLog log)
                          }
                        ]
                    }
                ]


logNameButton : Maybe Log -> Log -> Element Msg
logNameButton currentLog log =
    Input.button (Style.titleButton (currentLog == Just log))
        { onPress = Just (GetEvents log.id)
        , label = Element.text log.name
        }


eventsPanel : SharedState -> Model -> Element Msg
eventsPanel sharedState model =
    column [ spacing 20, height (px 490), width (px 300), Border.width 1 ]
        [ viewEvents sharedState model
        ]


viewEvents : SharedState -> Model -> Element Msg
viewEvents sharedState model =
    case sharedState.currentEventList of
        Nothing ->
            column [ spacing 12, padding 20, height (px 400) ]
                [ el [ Font.size 16, Font.bold ] (text "No events available")
                ]

        Just events ->
            let
                eventSum_ =
                    eventSum events
            in
            column [ spacing 12, padding 20, height (px 400) ]
                [ el [ Font.size 16, Font.bold ] (text "Events")
                , indexedTable [ spacing 4, Font.size 12 ]
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
                        , { header = el [ Font.bold ] (text "Value")
                          , width = px 40
                          , view = \k event -> el [ Font.size 12 ] (text <| event.value)
                          }
                        ]
                    }
                , row [ spacing 12, alignBottom ]
                    [ el [ Font.size 12 ] (text <| "Minutes: " ++ String.fromFloat eventSum_)
                    , el [ Font.size 12 ] (text <| "Hours: " ++ String.fromFloat (Utility.roundTo 1 (eventSum_ / 60)))
                    ]
                ]


eventSum : List Event -> Float
eventSum eventList =
    eventList
        |> List.map .value
        |> List.map (\str -> String.toFloat str |> Maybe.withDefault 0)
        |> List.sum


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


getLogsButton : Element Msg
getLogsButton =
    Input.button Style.button
        { onPress = Just (GetLogs 1)
        , label = el [ Font.size 14 ] (text "Get logs")
        }


getEventsButton : Element Msg
getEventsButton =
    Input.button Style.button
        { onPress = Just (GetEvents 1)
        , label = Element.text "Get events"
        }


submitEventButton : Element Msg
submitEventButton =
    Input.button Style.button
        { onPress = Just MakeEvent
        , label = Element.text "Submit"
        }


inputValue model =
    Input.text inputStyle
        { onChange = GotValueString
        , text = model.valueString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "Value")
        }


inputStyle =
    [ width (px 60), height (px 30), Background.color (Style.makeGrey 0.3), Font.color Style.white, Font.size 12 ]



--
-- GRAPHQL
--


logQuery : Int -> SelectionSet (List Log) RootQuery
logQuery userId =
    Query.logsForUser { userId = userId } logSelection


logSelection : SelectionSet Log Logger.Object.Log
logSelection =
    SelectionSet.map3 Log
        Logger.Object.Log.id
        Logger.Object.Log.name
        Logger.Object.Log.userId


getLogs : Int -> Cmd Msg
getLogs userId =
    logQuery userId
        |> Graphql.Http.queryRequest (Configuration.backend ++ "/graphiql")
        -- |> Graphql.Http.withHeader "authorization" "Bearer <your github bearer token>"
        |> Graphql.Http.send GotLogs



-- (Logger.Object.Log.logType |> SelectionSet.map fixOptions)
-- Logger.Object.Log.events


fixOptions =
    \optionals -> { optionals | logType = Present LogTypeValue.Float }



--
--type alias Log =
--    { id : Int
--    , name : String
--    , userId : Int
--    , logType : LogTypeValue
--    , events : List Event
--    }
--
--logTypeValueOfString : String -> Maybe LogTypeValue
--logTypeValueOfString str =
--    case str of
--        "INTEGER" ->
--            Just LogTypeValue.Integer
--
--        "FLOAT" ->
--            Just LogTypeValue.Float
--
--        "DATETIME" ->
--            Just LogTypeValue.Datetime
--
--        _ ->
--            Nothing


eventQuery : Int -> SelectionSet (List Event) RootQuery
eventQuery logId =
    Query.listEventsForLog { logId = logId } eventSelection


fpEventMutation :
    Int
    -> String
    -> SelectionSet decodesTo Logger.Object.Event
    -> SelectionSet (Maybe decodesTo) Graphql.Operation.RootMutation
fpEventMutation logId value =
    Mutation.createEvent { logId = logId, value = value }



-- SelectionSet decodesTo Graphql.Operation.RootMutation
-- makeEvent : Int -> Float -> Cmd Msg


makeEvent logId value =
    fpEventMutation logId value eventSelection
        |> Graphql.Http.mutationRequest (Configuration.backend ++ "/graphiql")
        |> Graphql.Http.send EventCreated


eventSelection : SelectionSet Event Logger.Object.Event
eventSelection =
    SelectionSet.map3 Event
        Logger.Object.Event.id
        Logger.Object.Event.value
        Logger.Object.Event.insertedAt


type alias EventListResponse =
    List Event


type alias LogListResponse =
    List Log


getEvents : Int -> Cmd Msg
getEvents logId =
    eventQuery logId
        |> Graphql.Http.queryRequest (Configuration.backend ++ "/graphiql")
        |> Graphql.Http.send GotEvents


query1 : SelectionSet () RootQuery
query1 =
    SelectionSet.empty
