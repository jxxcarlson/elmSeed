module Pages.Logs exposing
    ( Model
    , Msg(..)
    , getLogs
    , initModel
    , update
    , view
    )

--

import Common.BarGraph as BarGraph
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
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis.Tick as Tick
import LineChart.Axis.Ticks as Ticks
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import Logger.Enum.LogTypeValue as LogTypeValue exposing (LogTypeValue(..))
import Logger.Mutation as Mutation exposing (CreateEventRequiredArguments, CreateLogRequiredArguments, createEvent, createLog)
import Logger.Object
import Logger.Object.Event
import Logger.Object.Log
import Logger.Query as Query
import Logger.Scalar exposing (NaiveDateTime(..))
import SharedState exposing (Event, Log, SharedState, SharedStateUpdate(..))
import Time exposing (Posix)
import Utility.Data as Data
import Utility.DateTime exposing (offsetDateTimeStringByHours, rataDieFromNaiveDateTime)


type alias Model =
    { message : String
    , valueString : String
    , filterState : EventGrouping
    , logName : String
    , logFilterString : String
    , eventDateFilterString : String
    , timeZoneOffset : Int
    , timerState : TimerState
    , yScaleFactor : String
    }


initModel : Model
initModel =
    { message = "Nothing yet."
    , valueString = ""
    , filterState = NoGrouping
    , logName = ""
    , logFilterString = ""
    , eventDateFilterString = ""
    , timeZoneOffset = -4
    , timerState = TSInitial
    , yScaleFactor = "60.0"
    }


type TimerState
    = TSInitial
    | TSRunning
    | TSPaused


type TimerCommand
    = TCStart
    | TCPause
    | TCContinue
    | TCLog
    | TCReset


type EventGrouping
    = NoGrouping
    | GroupByDay



--
-- MSG
--


type Msg
    = NoOp
    | CreateLog
    | GetLogs
    | GotLogs (Result (Graphql.Http.Error LogListResponse) LogListResponse)
    | LogCreated (Result (Graphql.Http.Error (Maybe Log)) (Maybe Log))
    | GetEvents Int
    | GotEvents (Result (Graphql.Http.Error EventListResponse) EventListResponse)
    | EventCreated (Result (Graphql.Http.Error (Maybe Event)) (Maybe Event))
    | MakeEvent
    | GotValueString String
    | SetGroupFilter EventGrouping
    | GotLogName String
    | TC TimerCommand
    | GotYScaleFactor String
    | GotLogFilter String
    | GotEventDateFilter String



--    | FilterLogs
--    | UnFilterLogs


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoUpdate )

        GotLogName str ->
            ( { model | logName = str }, Cmd.none, NoUpdate )

        GotYScaleFactor str ->
            ( { model | yScaleFactor = str }, Cmd.none, NoUpdate )

        CreateLog ->
            case sharedState.currentUser of
                Nothing ->
                    ( model, Cmd.none, NoUpdate )

                Just user ->
                    ( model, makeLog user.id model.logName LogTypeValue.Float, NoUpdate )

        LogCreated (Ok result) ->
            let
                updatedLogList : List Log
                updatedLogList =
                    case ( sharedState.currentLogList, result ) of
                        ( Nothing, Just log ) ->
                            [ log ]

                        ( Just logList, Just log ) ->
                            logList ++ [ log ]

                        ( _, _ ) ->
                            []
            in
            ( model, Cmd.none, SharedState.UpdateSharedLogList updatedLogList )

        LogCreated (Err _) ->
            ( model, Cmd.none, NoUpdate )

        GetLogs ->
            case sharedState.currentUser of
                Nothing ->
                    ( { model | message = "No user signed in" }, Cmd.none, NoUpdate )

                Just user ->
                    ( model, getLogs user.id, NoUpdate )

        GotLogs (Ok logs) ->
            ( { model | message = "Logs received" }, Cmd.none, UpdateSharedLogList logs )

        GotLogs (Err _) ->
            ( { model | message = "Error getting logs" }, Cmd.none, NoUpdate )

        GotLogFilter str ->
            ( { model | logFilterString = str }, Cmd.none, UpdateCurrentLog Nothing )

        GotEventDateFilter str ->
            ( { model | eventDateFilterString = str }, Cmd.none, NoUpdate )

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
                    ( { model | message = "Event  created" }, Cmd.none, UpdateSharedEventList (eventList ++ [ event ]) )

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

        SetGroupFilter filterState ->
            ( { model | filterState = filterState }, Cmd.none, NoUpdate )

        TC timerCommand ->
            case timerCommand of
                TCStart ->
                    ( { model | timerState = TSRunning }, Cmd.none, BeginTimer )

                TCPause ->
                    ( { model | timerState = TSPaused }, Cmd.none, PauseTimer )

                TCContinue ->
                    ( { model | timerState = TSRunning }, Cmd.none, ContinueTimer )

                TCLog ->
                    let
                        cmd =
                            case sharedState.currentLog of
                                Nothing ->
                                    Cmd.none

                                Just log ->
                                    makeEvent log.id (String.fromFloat (Utility.roundTo 6 <| (sharedState.accumulatedTime + sharedState.elapsedTime) / 60.0))
                    in
                    ( { model | timerState = TSInitial }, cmd, ResetTimer )

                TCReset ->
                    ( { model | timerState = TSInitial }, Cmd.none, ResetTimer )


view : SharedState -> Model -> Element Msg
view sharedState model =
    column (Style.mainColumn fill fill ++ [ spacing 12, padding 40, Background.color (Style.makeGrey 0.9) ])
        [ row [ spacing 8 ]
            [ el [ Font.bold ] (text "Filter:")
            , inputLogNameFilter model
            , inputEventDateFilter model
            ]
        , row [ spacing 12 ]
            [ logListPanel sharedState model
            , eventsPanel sharedState model
            , chart sharedState model
            ]
        , column [ padding 8, Border.width 1, width (px 562), spacing 12 ]
            [ row [ spacing 140, alignBottom ]
                [ row [ spacing 12, Font.size 14 ]
                    [ getLogsButton
                    ]
                , row [ spacing 24, Font.size 14 ]
                    [ row [ spacing 8 ] [ submitEventButton, inputValue model ]
                    , row [ spacing 8 ] [ el [ Font.bold ] (text "Group:"), noFilterButton model, filterByDayButton model ]
                    ]
                ]
            , timerPanel sharedState model
            , newLogPanel model
            ]
        ]


{-| xxx
-}
timerPanel : SharedState -> Model -> Element Msg
timerPanel sharedState model =
    row [ spacing 12, Font.size 12, padding 8, Border.width 1, width (px 450) ]
        [ el [ Font.bold ] (text "TIMER")
        , startTimerButton
        , pauseTimerButton model
        , resetTimerButton
        , logTimerButton
        , row [ spacing 8 ]
            [ el [ Font.bold ] (text "Elapsed time")
            , el [ Font.size 16, Font.bold, padding 8, Font.color Style.red, Background.color Style.black ]
                (text <| timeStringFromFloat <| (sharedState.accumulatedTime + sharedState.elapsedTime) / scaleFactor)
            ]
        ]


scaleFactor =
    1


timeStringFromFloat : Float -> String
timeStringFromFloat t_ =
    let
        t =
            round t_

        s =
            modBy 60 t

        m =
            (t - s) // 60

        h =
            m // 60

        ss =
            String.pad 2 '0' (String.fromInt s)

        ms =
            String.pad 2 '0' (String.fromInt <| modBy 60 m)

        hs =
            String.pad 2 '0' (String.fromInt <| h)
    in
    hs ++ ":" ++ ms ++ ":" ++ ss


secondsFromPosix : Posix -> Int
secondsFromPosix p =
    p
        |> Time.posixToMillis
        |> toFloat
        |> (\x -> x / 1000.0)
        |> round


logListPanel : SharedState -> Model -> Element Msg
logListPanel sharedState model =
    column [ spacing 20, height (px 450), width (px 200), Border.width 1 ]
        [ viewLogs sharedState model
        ]


viewLogs : SharedState -> Model -> Element Msg
viewLogs sharedState model =
    case sharedState.currentLogList of
        Nothing ->
            column [ spacing 12, padding 20, height (px 400), Font.size 16 ]
                [ el [ Font.bold ] (text "No logs available")
                ]

        Just logs ->
            column [ spacing 12, padding 20, height (px 400) ]
                [ el [ Font.size 16, Font.bold ] (text "Logs")
                , indexedTable
                    [ spacing 4, Font.size 12 ]
                    { data = filterLogs model.logFilterString logs
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


filterLogs : String -> List Log -> List Log
filterLogs filter logs =
    List.filter (\log -> String.contains (String.toLower filter) (String.toLower log.name)) logs



--filterLogsButton : Element Msg
--filterLogsButton =
--    Input.button Style.button
--        { onPress = Just FilterLogs
--        , label = Element.text "Filter"
--        }
--
--
--unFilterLogsButton : Element Msg
--unFilterLogsButton =
--    Input.button Style.button
--        { onPress = Just UnFilterLogs
--        , label = Element.text "Show all"
--        }


logNameButton : Maybe Log -> Log -> Element Msg
logNameButton currentLog log =
    Input.button (Style.titleButton (currentLog == Just log))
        { onPress = Just (GetEvents log.id)
        , label = Element.text log.name
        }


noFilterButton : Model -> Element Msg
noFilterButton model =
    Input.button (Style.titleButton (model.filterState == NoGrouping))
        { onPress = Just (SetGroupFilter NoGrouping)
        , label = Element.text "None"
        }


filterByDayButton : Model -> Element Msg
filterByDayButton model =
    Input.button (Style.titleButton (model.filterState == GroupByDay))
        { onPress = Just (SetGroupFilter GroupByDay)
        , label = Element.text "By day"
        }


eventsPanel : SharedState -> Model -> Element Msg
eventsPanel sharedState model =
    column [ spacing 20, height (px 450), width (px 350), Border.width 1 ]
        [ viewEvents sharedState model
        ]



{-

   case model.eventDateFilterString of
          Nothing ->

      let
          todayND =
              Utility.DateTime.naiveDateStringFromPosix sharedState.currentTime |> Debug.log "TODAY"

          inLastNDaysBeforeDate endDate interval
      in
-}


viewEvents : SharedState -> Model -> Element Msg
viewEvents sharedState model =
    case sharedState.currentEventList of
        Nothing ->
            column [ spacing 12, padding 20, height (px 500) ]
                [ el [ Font.size 16, Font.bold ] (text "No events available")
                ]

        Just events_ ->
            let
                eventSum_ =
                    eventSum events_

                events =
                    case model.filterState of
                        NoGrouping ->
                            Data.correctTimeZone model.timeZoneOffset events_

                        GroupByDay ->
                            Data.eventsByDay model.timeZoneOffset events_
            in
            column [ spacing 12, padding 20, height (px 430), scrollbarY ]
                [ el [ Font.size 16, Font.bold ] (text "Events")
                , indexedTable [ spacing 4, Font.size 12 ]
                    { data = events
                    , columns =
                        [ { header = el [ Font.bold ] (text "index")
                          , width = px 40
                          , view = \k event -> el [ Font.size 12 ] (text <| String.fromInt <| k + 1)
                          }
                        , { header = el [ Font.bold ] (text "Day")
                          , width = px 40
                          , view = \k event -> el [ Font.size 12 ] (text <| String.fromInt <| (\x -> x - 737147) <| rataDieFromNaiveDateTime <| (\(NaiveDateTime str) -> str) <| event.insertedAt)
                          }
                        , { header = el [ Font.bold ] (text "Date")
                          , width = px 80
                          , view = \k event -> el [ Font.size 12 ] (text <| dateStringOfDateTimeString <| (\(NaiveDateTime str) -> str) <| event.insertedAt)
                          }
                        , { header = el [ Font.bold ] (text "Time")
                          , width = px 80
                          , view = \k event -> el [ Font.size 12 ] (text <| timeStringOfDateTimeString <| (\(NaiveDateTime str) -> str) <| event.insertedAt)
                          }
                        , { header = el [ Font.bold ] (text "Value")
                          , width = px 40
                          , view = \k event -> el [ Font.size 12 ] (text <| formatValue model.yScaleFactor 2 <| event.value)
                          }
                        ]
                    }
                , row [ spacing 12, alignBottom ]
                    [ el [ Font.size 12 ] (text <| "Minutes: " ++ String.fromFloat (Utility.roundTo 1 eventSum_))
                    , el [ Font.size 12 ] (text <| "Hours: " ++ String.fromFloat (Utility.roundTo 1 (eventSum_ / 60)))
                    ]
                ]


formatValue : String -> Int -> String -> String
formatValue scaleFactor_ k yString =
    case String.toFloat yString of
        Nothing ->
            yString

        Just y ->
            case String.toFloat scaleFactor_ of
                Nothing ->
                    yString

                Just f_ ->
                    Utility.roundTo k (y / f_)
                        |> String.fromFloat


eventSum : List Event -> Float
eventSum eventList =
    eventList
        |> List.map .value
        |> List.map (\str -> String.toFloat str |> Maybe.withDefault 0)
        |> List.sum


dateStringOfDateTimeString : String -> String
dateStringOfDateTimeString str =
    str
        |> String.split "T"
        |> List.head
        |> Maybe.withDefault "-"


timeStringOfDateTimeString : String -> String
timeStringOfDateTimeString str =
    str
        |> String.split "T"
        |> List.reverse
        |> List.head
        |> Maybe.withDefault "-"



--
-- BUTTON
--


{-| xxx
-}
startTimerButton : Element Msg
startTimerButton =
    Input.button Style.smallButton
        { onPress = Just (TC TCStart)
        , label = el [ Font.size 12 ] (text "Start")
        }


pauseTimerButton : Model -> Element Msg
pauseTimerButton model =
    case model.timerState of
        TSPaused ->
            Input.button Style.smallButton
                { onPress = Just (TC TCContinue)
                , label = el [ Font.size 12 ] (text "Cont")
                }

        _ ->
            Input.button Style.smallButton
                { onPress = Just (TC TCPause)
                , label = el [ Font.size 12 ] (text "Pause")
                }


resetTimerButton : Element Msg
resetTimerButton =
    Input.button Style.smallButton
        { onPress = Just (TC TCReset)
        , label = el [ Font.size 12 ] (text "Reset")
        }


logTimerButton : Element Msg
logTimerButton =
    Input.button Style.smallButton
        { onPress = Just (TC TCLog)
        , label = el [ Font.size 12 ] (text "Log")
        }


getLogsButton : Element Msg
getLogsButton =
    Input.button Style.button
        { onPress = Just GetLogs
        , label = el [ Font.size 14 ] (text "Get logs")
        }


submitEventButton : Element Msg
submitEventButton =
    Input.button Style.button
        { onPress = Just MakeEvent
        , label = Element.text "Submit"
        }


createLButton : Model -> Element Msg
createLButton model =
    Input.button Style.button
        { onPress = Just CreateLog
        , label = el [ Font.size 14 ] (text "Create log with name")
        }


newLogPanel model =
    row [ spacing 12, Font.size 12 ]
        [ createLButton model
        , inputLogName model
        ]



--
-- INPUT FIELDS
--


inputValue model =
    Input.text inputStyle
        { onChange = GotValueString
        , text = model.valueString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputLogNameFilter model =
    Input.text inputStyle
        { onChange = GotLogFilter
        , text = model.logFilterString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputEventDateFilter model =
    Input.text inputStyle
        { onChange = GotEventDateFilter
        , text = model.logFilterString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputLogName model =
    Input.text (inputStyle ++ [ width (px 200) ])
        { onChange = GotLogName
        , text = model.logName
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputYScaleFactor model =
    Input.text (inputStyle ++ [ width (px 60) ])
        { onChange = GotYScaleFactor
        , text = model.yScaleFactor
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputStyle =
    [ width (px 60)
    , height (px 30)
    , Background.color (Style.makeGrey 0.8)
    , Font.color Style.black
    , Font.size 12
    , Border.width 2
    ]



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
        -- |> Graphql.Http.withHeader "Access-Control-Allow-Origin" "http://localhost:4000"
        |> Graphql.Http.send GotLogs


logMutation :
    Int
    -> String
    -> LogTypeValue
    -> SelectionSet decodesTo Logger.Object.Log
    -> SelectionSet (Maybe decodesTo) Graphql.Operation.RootMutation
logMutation userId name logType =
    Mutation.createLog { userId = userId, logType = logType, name = name }


makeLog userId name logType =
    logMutation userId name logType logSelection
        |> Graphql.Http.mutationRequest (Configuration.backend ++ "/graphiql")
        |> Graphql.Http.send LogCreated


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



--
-- CHART
--


chart1 : SharedState -> Model -> Element msg
chart1 sharedState model =
    case sharedState.currentEventList of
        Nothing ->
            Element.none

        Just eventList_ ->
            let
                events =
                    case model.filterState of
                        NoGrouping ->
                            Data.correctTimeZone model.timeZoneOffset eventList_

                        GroupByDay ->
                            Data.eventsByDay model.timeZoneOffset eventList_
            in
            column [ Font.size 12 ]
                [ LineChart.viewCustom chartConfig
                    [ LineChart.line Colors.blue Dots.square "Events" (prepareData events) ]
                    |> Element.html
                ]


chart : SharedState -> Model -> Element Msg
chart sharedState model =
    case sharedState.currentEventList of
        Nothing ->
            Element.none

        Just eventList_ ->
            let
                events =
                    case model.filterState of
                        NoGrouping ->
                            Data.correctTimeZone model.timeZoneOffset eventList_

                        GroupByDay ->
                            Data.eventsByDay model.timeZoneOffset eventList_
            in
            column [ Font.size 12, spacing 36, moveRight 40, width (px 450) ]
                [ row [] [ BarGraph.asHtml gA (prepareData2 (getScaleFactor model) events) |> Element.html ]
                , row [ spacing 8 ]
                    [ el [] (text "Scale factor")
                    , inputYScaleFactor model
                    ]
                ]


getScaleFactor : Model -> Float
getScaleFactor model =
    case String.toFloat model.yScaleFactor of
        Nothing ->
            1.0

        Just f ->
            f


gA =
    { dx = 10
    , color = "blue"
    , barHeight = 200
    , graphWidth = 400
    }


prepareData : List Event -> List Data
prepareData eventList =
    eventList
        |> List.map .value
        |> List.indexedMap Tuple.pair
        |> List.map dataFromPair


prepareData2 : Float -> List Event -> List Float
prepareData2 scaleFactor_ eventList =
    eventList
        |> List.map .value
        |> List.map String.toFloat
        |> List.map (Maybe.withDefault -999.0)
        |> List.map (\x -> x / scaleFactor_)


dataFromPair : ( Int, String ) -> Data
dataFromPair ( a, b ) =
    { index = toFloat a, value = String.toFloat b |> Maybe.withDefault 0 }


type alias Data =
    { index : Float
    , value : Float
    }


chartConfig : LineChart.Config Data msg
chartConfig =
    { x = Axis.full 800 "index" .index
    , y = Axis.full 400 "Value" .value
    , container = Container.default "line-chart-1"
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = Legends.none
    , events = Events.default
    , junk = Junk.default
    , grid = Grid.default
    , area = Area.stacked 0.3 -- Changed from the default!
    , line = Line.wider 2
    , dots = Dots.default
    }
