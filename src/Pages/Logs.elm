module Pages.Logs exposing
    ( Model
    , Msg(..)
    , getLogs
    , initModel
    , makeLog
    , update
    , view
    )

--

import Common.Style as Style
import Configuration
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Graph exposing (Option(..))
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
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
import Utility.TypedTime as TypedTime exposing (TypedTime(..), Unit(..))



-- exposing (TypedTime(..), Unit(..), convertTo, timeAsStringWithUnit)


authorizationHeader =
    "Bearer 1f26cbfff22e414099a33666d9f3e699"


type alias Model =
    { message : String
    , valueString : String
    , filterState : EventGrouping
    , dateFilter : DateFilter
    , logName : String
    , updatedLogName : String
    , logFilterString : String
    , eventDateFilterString : String
    , timeZoneOffset : Int
    , timerState : TimerState
    , yScaleFactor : String
    , inputUnit : Unit
    , outputUnit : Unit
    , appMode : AppMode
    , phoneAppMode : PhoneAppMode
    , currentEvent : Maybe Event
    }


initModel : Model
initModel =
    { message = "Nothing yet."
    , valueString = ""
    , filterState = NoGrouping
    , dateFilter = NoDateFilter
    , logName = ""
    , updatedLogName = ""
    , logFilterString = ""
    , eventDateFilterString = ""
    , timeZoneOffset = -4
    , timerState = TSInitial
    , yScaleFactor = "60.0"
    , inputUnit = Seconds
    , outputUnit = Minutes
    , appMode = Logging
    , phoneAppMode = ShowingLogs
    , currentEvent = Nothing
    }


type AppMode
    = Logging
    | Editing


type PhoneAppMode
    = ShowingLogs
    | ShowingEvents
    | ShowingChart


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


type DateFilter
    = NoDateFilter
    | FilterByLast Int



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
    | DeleteEvent Int
    | EventDeleted (Result (Graphql.Http.Error (Maybe Event)) (Maybe Event))
      --
    | GotLogFilter String
    | GotEventDateFilter String
    | SetGroupFilter EventGrouping
    | SetDateFilter DateFilter
      --
    | GotLogName String
    | GotUpdatedLogName String
    | TC TimerCommand
    | GotYScaleFactor String
      --
    | SetUnits Unit
    | SetAppMode AppMode
    | SetCurrentEvent Event
      --
    | SetPhoneAppMode PhoneAppMode



--    | FilterLogs
--    | UnFilterLogs


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoUpdate )

        GotLogName str ->
            ( { model | logName = str }, Cmd.none, NoUpdate )

        GotUpdatedLogName str ->
            ( { model | updatedLogName = str }, Cmd.none, NoUpdate )

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
            ( { model | phoneAppMode = ShowingLogs, message = "Logs received" }, Cmd.none, UpdateSharedLogList logs )

        GotLogs (Err _) ->
            ( { model | message = "Error getting logs" }, Cmd.none, NoUpdate )

        GotLogFilter str ->
            ( { model | logFilterString = str }, Cmd.none, UpdateCurrentLog Nothing )

        GotEventDateFilter str ->
            case String.toInt str of
                Nothing ->
                    ( { model | dateFilter = NoDateFilter, eventDateFilterString = str }, Cmd.none, NoUpdate )

                Just k ->
                    case k <= 0 of
                        True ->
                            ( { model | dateFilter = NoDateFilter, eventDateFilterString = str }, Cmd.none, NoUpdate )

                        False ->
                            ( { model | dateFilter = FilterByLast k, eventDateFilterString = str }, Cmd.none, NoUpdate )

        GetEvents logId ->
            let
                maybeLog =
                    Maybe.map (List.filter (\log -> log.id == logId)) sharedState.currentLogList
                        |> Maybe.andThen List.head
            in
            ( { model | phoneAppMode = ShowingEvents, currentEvent = Nothing }, getEvents logId, UpdateCurrentLog maybeLog )

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
                    case TypedTime.decodeHM model.valueString of
                        Nothing ->
                            ( { model | message = "Bad format for time" }, Cmd.none, NoUpdate )

                        Just duration ->
                            ( { model | message = "Making new event for log " ++ String.fromInt log.id }
                            , makeEvent log.id duration
                            , NoUpdate
                            )

        DeleteEvent id ->
            ( model, deleteEvent id, NoUpdate )

        EventDeleted (Ok maybeEvent) ->
            case maybeEvent of
                Nothing ->
                    ( { model | message = "No event deleted" }, Cmd.none, NoUpdate )

                Just event ->
                    ( { model | message = "Events received" }, Cmd.none, RemoveEventFromSharedEventList event.id )

        EventDeleted (Err _) ->
            ( { model | message = "Error deleting event" }, Cmd.none, NoUpdate )

        SetGroupFilter filterState ->
            ( { model | filterState = filterState }, Cmd.none, NoUpdate )

        SetDateFilter dateFilter_ ->
            ( { model | dateFilter = dateFilter_ }, Cmd.none, NoUpdate )

        SetUnits unit ->
            ( { model | outputUnit = unit }, Cmd.none, NoUpdate )

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
                                    makeEvent log.id (TypedTime.convertScalarToSecondsWithUnit model.inputUnit (sharedState.accumulatedTime + sharedState.elapsedTime))
                    in
                    ( { model | timerState = TSInitial }, cmd, ResetTimer )

                TCReset ->
                    ( { model | timerState = TSInitial }, Cmd.none, ResetTimer )

        SetAppMode nextAppMode ->
            case ( sharedState.currentUser, sharedState.currentLogList ) of
                ( Nothing, _ ) ->
                    ( { model | message = "No user signed in" }, Cmd.none, NoUpdate )

                ( Just user, Nothing ) ->
                    ( { model | appMode = nextAppMode, filterState = NoGrouping }, getLogs user.id, NoUpdate )

                ( Just _, Just _ ) ->
                    ( { model | appMode = nextAppMode, filterState = NoGrouping }, Cmd.none, NoUpdate )

        SetCurrentEvent event_ ->
            ( { model | currentEvent = Just event_ }, Cmd.none, NoUpdate )

        SetPhoneAppMode phoneAppMode ->
            ( { model | phoneAppMode = phoneAppMode }, Cmd.none, NoUpdate )



--
-- VIEW
--


view : SharedState -> Model -> Element Msg
view sharedState model =
    case classifyDevice { width = sharedState.windowWidth, height = sharedState.windowHeight } |> .class of
        Phone ->
            phoneView sharedState model

        _ ->
            mainView sharedState model



--
-- MAINVIEW
--


mainView : SharedState -> Model -> Element Msg
mainView sharedState model =
    column (Style.mainColumn fill fill ++ [ spacing 12, padding 40, Background.color (Style.makeGrey 0.9) ])
        [ filterPanel sharedState model
        , row [ spacing 12 ]
            [ logListPanel sharedState model
            , eventListDisplay sharedState model
            , case model.appMode of
                Logging ->
                    eventPanel sharedState model

                Editing ->
                    editPanel sharedState model
            ]
        , controlPanel sharedState model
        ]



--
-- PHONEVIEW
--


phoneView : SharedState -> Model -> Element Msg
phoneView sharedState model =
    column (Style.mainColumn fill fill ++ [ spacing 12, padding 40, Background.color (Style.makeGrey 0.9) ])
        [ case model.phoneAppMode of
            ShowingLogs ->
                filterLogsPanel sharedState model

            ShowingEvents ->
                filterEventsPanel sharedState model

            ShowingChart ->
                Element.none
        , row [ spacing 12 ]
            [ case model.phoneAppMode of
                ShowingLogs ->
                    logListPanelForPhone sharedState model

                ShowingEvents ->
                    eventListDisplayPhone sharedState model

                ShowingChart ->
                    chartPanelForPhone sharedState model
            ]
        , case model.phoneAppMode of
            ShowingLogs ->
                controlPanelForPhone sharedState model

            ShowingEvents ->
                newEventPanel 300 sharedState model

            ShowingChart ->
                Element.none
        ]


filterLogsPanel sharedState model =
    row [ spacing 8 ]
        [ el [ Font.bold ] (text "Filter:")
        , inputLogNameFilter model

        --  , row [ alignRight, moveRight 36, spacing 12 ] [ editModeButton sharedState model, logModeButton model ]
        ]


filterEventsPanel sharedState model =
    row [ spacing 8 ]
        [ el [ Font.bold ] (text "Since:")
        , inputEventDateFilter model
        , goToChartButton model
        ]



--
-- EDIT PANEL
--


editPanel sharedState model =
    column [ spacing 12, width (px 300), alignTop ]
        [ el [ Font.size 16, Font.bold, alignTop ] (text "Edit Panel")
        , logEditPanel sharedState model
        , logEventPanel sharedState model
        ]


logEditPanel sharedState model =
    column [ padding 12, Border.width 1, spacing 12 ]
        [ editLogNameInput model
        , updateLogButton
        , deleteLogButton
        ]


logEventPanel sharedState model =
    case model.currentEvent of
        Nothing ->
            Element.none

        Just evt ->
            column [ padding 12, Border.width 1 ]
                [ deleteEventButton evt
                ]


editLogNameInput model =
    Input.text (inputStyle ++ [ width (px 200) ])
        { onChange = GotUpdatedLogName
        , text = model.updatedLogName
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 16, moveDown 8 ] (text "New log name")
        }


updateLogButton : Element Msg
updateLogButton =
    Input.button Style.button
        { onPress = Just NoOp
        , label = Element.text "Update log"
        }


goToChartButton : Model -> Element Msg
goToChartButton model =
    Input.button Style.button
        { onPress = Just (SetPhoneAppMode ShowingChart)
        , label = Element.text "Chart"
        }


goToEventsButton : Model -> Element Msg
goToEventsButton model =
    Input.button Style.button
        { onPress = Just (SetPhoneAppMode ShowingEvents)
        , label = Element.text "Events"
        }


phoneAppModeString : Model -> String
phoneAppModeString model =
    case model.phoneAppMode of
        ShowingLogs ->
            "Logs"

        ShowingEvents ->
            "Events"

        ShowingChart ->
            "Chart"


deleteLogButton : Element Msg
deleteLogButton =
    Input.button Style.button
        { onPress = Just NoOp
        , label = Element.text "Remove log"
        }


deleteEventButton : Event -> Element Msg
deleteEventButton event =
    Input.button Style.button
        { onPress = Just (DeleteEvent event.id)
        , label = Element.text <| "Remove event " ++ String.fromInt event.id
        }



--
-- CONTINUE
--


filterPanel sharedState model =
    row [ spacing 8 ]
        [ el [ Font.bold ] (text "Filter:")
        , inputLogNameFilter model
        , el [ Font.bold ] (text "Since:")
        , inputEventDateFilter model
        , row [ alignRight, moveRight 36, spacing 12 ] [ editModeButton sharedState model, logModeButton model ]
        ]


controlPanel sharedState model =
    column [ padding 8, Border.width 1, width (px 562), spacing 12 ]
        [ newLogPanel model
        , el [ Font.size 14 ] (text <| model.message)
        , el [ Font.size 11 ] (text <| "Server: " ++ Configuration.backend)
        ]


controlPanelForPhone sharedState model =
    column [ padding 8, Border.width 1, width (px 300), spacing 12 ]
        [ newLogPanelForPhone model
        , el [ Font.size 14 ] (text <| model.message)
        , el [ Font.size 11 ] (text <| "Server: " ++ Configuration.backend)
        ]


largeElapsedTimePanel : SharedState -> Model -> Element Msg
largeElapsedTimePanel sharedState model =
    column [ spacing 12 ]
        [ timerDisplay sharedState model
        , timerControls sharedState model
        ]


timerControls : SharedState -> Model -> Element Msg
timerControls sharedState model =
    row [ spacing 12, Font.size 12, width fill ]
        [ startTimerButton
        , pauseTimerButton model
        , resetTimerButton
        , logTimerButton
        ]


timerDisplay sharedState model =
    row [ spacing 8 ]
        [ el [ Font.size 36, Font.bold, padding 8, Font.color Style.red, Background.color Style.black ]
            (text <| timeStringFromFloat <| (sharedState.accumulatedTime + sharedState.elapsedTime) / scaleFactor)
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
    column [ spacing 20, height (px 450), width (px 350), Border.width 1 ]
        [ viewLogs sharedState model
        ]


logListPanelForPhone : SharedState -> Model -> Element Msg
logListPanelForPhone sharedState model =
    column [ spacing 20, height (px 350), width (px 300), Border.width 1 ]
        [ viewLogsForPhone sharedState model
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


viewLogsForPhone : SharedState -> Model -> Element Msg
viewLogsForPhone sharedState model =
    case sharedState.currentLogList of
        Nothing ->
            column [ spacing 12, padding 20, height (px 400), Font.size 16 ]
                [ el [ Font.bold ] (text "No logs available")
                ]

        Just logs ->
            column [ spacing 12, padding 20, height (px 400) ]
                [ indexedTable
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


logNameButton : Maybe Log -> Log -> Element Msg
logNameButton currentLog log =
    Input.button (Style.titleButton (currentLog == Just log))
        { onPress = Just (GetEvents log.id)
        , label = Element.text log.name
        }


noFilterButton : Model -> Element Msg
noFilterButton model =
    Input.button (Style.activeButton (model.filterState == NoGrouping))
        { onPress = Just (SetGroupFilter NoGrouping)
        , label = el [ Font.size 12 ] (text "None")
        }


filterByDayButton : Model -> Element Msg
filterByDayButton model =
    Input.button (Style.activeButton (model.filterState == GroupByDay))
        { onPress = Just (SetGroupFilter GroupByDay)
        , label = el [ Font.size 12 ] (text "By day")
        }


eventListDisplay : SharedState -> Model -> Element Msg
eventListDisplay sharedState model =
    column [ spacing 20, height (px 450), width (px 350), Border.width 1 ]
        [ viewEvents sharedState model
        ]


eventListDisplayPhone : SharedState -> Model -> Element Msg
eventListDisplayPhone sharedState model =
    column [ spacing 20, height (px 300), width (px 300), Border.width 1 ]
        [ viewEvents sharedState model
        ]


groupingFilter : Int -> EventGrouping -> List Event -> List Event
groupingFilter timeZoneOffset eventGrouping eventList =
    case eventGrouping of
        NoGrouping ->
            Data.correctTimeZone timeZoneOffset eventList

        GroupByDay ->
            Data.eventsByDay timeZoneOffset eventList


dateFilter : String -> DateFilter -> List Event -> List Event
dateFilter todayAsString dateFilter_ eventList =
    case dateFilter_ of
        NoDateFilter ->
            eventList

        FilterByLast k ->
            List.filter (\event -> Utility.DateTime.inLastNDaysBeforeDate todayAsString k (naiveDateTimeValue event.insertedAt) == Just True) eventList


naiveDateTimeValue : NaiveDateTime -> String
naiveDateTimeValue (NaiveDateTime str) =
    str



-- model.timeZoneOffset


viewEvents : SharedState -> Model -> Element Msg
viewEvents sharedState model =
    case sharedState.currentEventList of
        Nothing ->
            column [ spacing 12, padding 20, height (px 500) ]
                [ el [ Font.size 16, Font.bold ] (text "No events available")
                ]

        Just events1 ->
            let
                today =
                    Utility.DateTime.naiveDateStringFromPosix sharedState.currentTime

                events2 =
                    dateFilter today model.dateFilter events1

                eventSum_ =
                    eventSum events2

                events : List Event
                events =
                    groupingFilter model.timeZoneOffset model.filterState events2

                nEvents =
                    List.length events |> toFloat

                average =
                    TypedTime.multiply (1.0 / nEvents) eventSum_
            in
            column [ spacing 12, padding 20, height (px 430), scrollbarY ]
                [ el [ Font.size 16, Font.bold ] (text (Maybe.map .name sharedState.currentLog |> Maybe.withDefault "XXX"))
                , indexedTable [ spacing 4, Font.size 12 ]
                    { data = events
                    , columns =
                        [ { header = el [ Font.bold ] (text "idx")
                          , width = px (indexWidth model.appMode)
                          , view = indexButton model
                          }
                        , { header = el [ Font.bold ] (text "Date")
                          , width = px 80

                          --, view = \k event -> el [ Font.size 12 ] (text <| dateStringOfDateTimeString <| (\(NaiveDateTime str) -> str) <| event.insertedAt)
                          , view = \k event -> el [ Font.size 12 ] (text <| dateStringOfDateTimeString <| (\ndt -> Utility.DateTime.humanDateFromNaiveDateTime ndt) <| event.insertedAt)
                          }
                        , { header = el [ Font.bold ] (text "Time")
                          , width = px 80
                          , view = \k event -> el [ Font.size 12 ] (text <| timeStringOfDateTimeString <| (\(NaiveDateTime str) -> str) <| event.insertedAt)
                          }
                        , { header = el [ Font.bold ] (text "Value")
                          , width = px 40
                          , view = \k event -> el [ Font.size 12 ] (text <| TypedTime.timeAsStringWithUnit Minutes (TypedTime Seconds event.value))
                          }
                        ]
                    }
                , row [ spacing 24, alignBottom, alignRight ]
                    [ el [ moveLeft 10, Font.size 16, Font.bold ] (text <| "Average: " ++ TypedTime.timeAsStringWithUnit Minutes average)
                    , el [ moveLeft 10, Font.size 16, Font.bold ] (text <| "Total: " ++ TypedTime.timeAsStringWithUnit Minutes eventSum_)
                    ]
                ]


indexWidth : AppMode -> Int
indexWidth appMode =
    case appMode of
        Logging ->
            30

        Editing ->
            60


indexButton : Model -> Int -> Event -> Element Msg
indexButton model k event =
    case model.appMode of
        Logging ->
            el [ Font.size 12 ] (text <| String.fromInt <| k + 1)

        Editing ->
            setCurrentEventButton model event k



-- el [ Font.size 12 ] (text <| String.fromInt <| k + 1)


setCurrentEventButton : Model -> Event -> Int -> Element Msg
setCurrentEventButton model event index =
    Input.button (Style.titleButton (Just event == model.currentEvent))
        { onPress = Just (SetCurrentEvent event)
        , label = el [ Font.bold ] (Element.text <| String.fromInt index ++ ": " ++ String.fromInt event.id)
        }


formatValue : Model -> Float -> String
formatValue model v =
    TypedTime.timeAsStringWithUnit model.outputUnit (TypedTime Seconds v)


formatValueForUnit : Unit -> Float -> String
formatValueForUnit unit v =
    TypedTime.timeAsStringWithUnit unit (TypedTime Seconds v)


eventSum : List Event -> TypedTime
eventSum eventList =
    eventList
        |> List.map .value
        |> List.sum
        |> TypedTime Seconds


dateStringOfDateTimeString : String -> String
dateStringOfDateTimeString str =
    case str == "1900-01-01" of
        True ->
            ""

        False ->
            str
                |> String.split "T"
                |> List.head
                |> Maybe.withDefault "-"


timeStringOfDateTimeString : String -> String
timeStringOfDateTimeString str =
    case str == "1900-01-01" of
        True ->
            ""

        False ->
            str
                |> String.split "T"
                |> List.reverse
                |> List.head
                |> Maybe.withDefault "-"



--
-- BUTTON
--


editModeButton : SharedState -> Model -> Element Msg
editModeButton sharedState model =
    case sharedState.currentLog of
        Nothing ->
            Element.none

        Just _ ->
            Input.button (Style.activeButton (model.appMode == Editing))
                { onPress = Just (SetAppMode Editing)
                , label = el [ Font.size 16 ] (text "Edit")
                }


logModeButton : Model -> Element Msg
logModeButton model =
    Input.button (Style.activeButton (model.appMode == Logging))
        { onPress = Just (SetAppMode Logging)
        , label = el [ Font.size 16 ] (text "Logs")
        }


editModeLabel : AppMode -> String
editModeLabel appMode =
    case appMode of
        Editing ->
            "Edit Logs, Events"

        Logging ->
            "Log events"


setMinutesButton : Model -> Element Msg
setMinutesButton model =
    Input.button (Style.activeButton (model.outputUnit == Minutes))
        { onPress = Just (SetUnits Minutes)
        , label = el [ Font.size 12 ] (text "Minutes")
        }


setHoursButton : Model -> Element Msg
setHoursButton model =
    Input.button (Style.activeButton (model.outputUnit == Hours))
        { onPress = Just (SetUnits Hours)
        , label = el [ Font.size 12 ] (text "Hours")
        }


{-| xxx
-}
startTimerButton : Element Msg
startTimerButton =
    Input.button Style.button
        { onPress = Just (TC TCStart)
        , label = el [ Font.size 14 ] (text "Start")
        }


pauseTimerButton : Model -> Element Msg
pauseTimerButton model =
    case model.timerState of
        TSPaused ->
            Input.button Style.smallButton
                { onPress = Just (TC TCContinue)
                , label = el [ Font.size 14 ] (text "Cont")
                }

        _ ->
            Input.button Style.button
                { onPress = Just (TC TCPause)
                , label = el [ Font.size 14 ] (text "Pause")
                }


resetTimerButton : Element Msg
resetTimerButton =
    Input.button Style.button
        { onPress = Just (TC TCReset)
        , label = el [ Font.size 14 ] (text "Reset")
        }


logTimerButton : Element Msg
logTimerButton =
    Input.button Style.button
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
        , label = Element.text "New event"
        }


createLButton : Model -> Element Msg
createLButton model =
    Input.button Style.button
        { onPress = Just CreateLog
        , label = el [ Font.size 14 ] (text "Create log with name")
        }


createLButtonForPhone : Model -> Element Msg
createLButtonForPhone model =
    Input.button Style.button
        { onPress = Just CreateLog
        , label = el [ Font.size 14 ] (text "Create log")
        }


newLogPanel model =
    row [ spacing 12, Font.size 12 ]
        [ createLButton model
        , inputLogName model 200
        ]


newLogPanelForPhone model =
    row [ spacing 12, Font.size 12 ]
        [ createLButtonForPhone model
        , inputLogName model 100
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
        , text = model.eventDateFilterString
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8 ] (text "")
        }


inputLogName : Model -> Int -> Element Msg
inputLogName model w =
    Input.text (inputStyle ++ [ width (px w) ])
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
--
-- GET LOGS
--


getLogs : Int -> Cmd Msg
getLogs userId =
    logQuery userId
        |> Graphql.Http.queryRequest (Configuration.backend ++ "/graphiql")
        |> Graphql.Http.withHeader "authorization" authorizationHeader
        |> Graphql.Http.withHeader "Access-Control-Allow-Origin" Configuration.backend
        |> Graphql.Http.send GotLogs


logQuery : Int -> SelectionSet (List Log) RootQuery
logQuery userId =
    Query.logsForUser { userId = userId } logSelection


logSelection : SelectionSet Log Logger.Object.Log
logSelection =
    SelectionSet.map3 Log
        Logger.Object.Log.id
        Logger.Object.Log.name
        Logger.Object.Log.userId



--
-- GET EVENTS
--


getEvents : Int -> Cmd Msg
getEvents logId =
    eventQuery logId
        |> Graphql.Http.queryRequest (Configuration.backend ++ "/graphiql")
        |> Graphql.Http.withHeader "authorization" authorizationHeader
        |> Graphql.Http.withHeader "Access-Control-Allow-Origin" Configuration.backend
        |> Graphql.Http.send GotEvents


eventQuery : Int -> SelectionSet (List Event) RootQuery
eventQuery logId =
    Query.listEventsForLog { logId = logId } eventSelection


floatValueFromString : Unit -> String -> Float
floatValueFromString inputUnit str =
    str
        |> String.toFloat
        |> Maybe.withDefault 0
        |> TypedTime.convertScalarToSecondsWithUnit inputUnit



--
-- CREATE LOG
--


makeLog : Int -> String -> LogTypeValue -> Cmd Msg
makeLog userId name logType =
    logMutation userId name logType logSelection
        |> Graphql.Http.mutationRequest (Configuration.backend ++ "/graphiql")
        |> Graphql.Http.withHeader "authorization" authorizationHeader
        |> Graphql.Http.withHeader "Access-Control-Allow-Origin" Configuration.backend
        |> Graphql.Http.send LogCreated


logMutation :
    Int
    -> String
    -> LogTypeValue
    -> SelectionSet decodesTo Logger.Object.Log
    -> SelectionSet (Maybe decodesTo) Graphql.Operation.RootMutation
logMutation userId name logType =
    Mutation.createLog { userId = userId, logType = logType, name = name }



--
-- CREATE EVENT
--


makeEvent : Int -> Float -> Cmd Msg
makeEvent logId value =
    fpEventMutation logId value eventSelection
        |> Graphql.Http.mutationRequest (Configuration.backend ++ "/graphiql")
        |> Graphql.Http.withHeader "authorization" authorizationHeader
        |> Graphql.Http.withHeader "Access-Control-Allow-Origin" Configuration.backend
        |> Graphql.Http.send EventCreated


fpEventMutation :
    Int
    -> Float
    -> SelectionSet decodesTo Logger.Object.Event
    -> SelectionSet (Maybe decodesTo) Graphql.Operation.RootMutation
fpEventMutation logId value =
    Mutation.createEvent { logId = logId, value = value }



--
-- DELETE EVENT
--


deleteEvent : Int -> Cmd Msg
deleteEvent id =
    eventMutation id eventSelection
        |> Graphql.Http.mutationRequest (Configuration.backend ++ "/graphiql")
        |> Graphql.Http.withHeader "authorization" authorizationHeader
        |> Graphql.Http.withHeader "Access-Control-Allow-Origin" Configuration.backend
        |> Graphql.Http.send EventDeleted


eventMutation :
    Int
    -> SelectionSet decodesTo Logger.Object.Event
    -> SelectionSet (Maybe decodesTo) Graphql.Operation.RootMutation
eventMutation id =
    Mutation.deleteEvent { id = id }


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


query1 : SelectionSet () RootQuery
query1 =
    SelectionSet.empty



--
-- CHART
--


eventPanel : SharedState -> Model -> Element Msg
eventPanel sharedState model =
    case sharedState.currentEventList of
        Nothing ->
            Element.none

        Just eventList_ ->
            let
                today =
                    Utility.DateTime.naiveDateStringFromPosix sharedState.currentTime

                events2 =
                    dateFilter today model.dateFilter eventList_

                events =
                    case model.filterState of
                        NoGrouping ->
                            Data.correctTimeZone model.timeZoneOffset events2

                        GroupByDay ->
                            Data.eventsByDay model.timeZoneOffset events2
            in
            column [ Font.size 12, spacing 36, moveRight 40, width (px 450) ]
                [ row [ moveLeft 40 ] [ Graph.barChart (gA model) (prepareData (getScaleFactor model) events) |> Element.html ]
                , row [ spacing 16 ]
                    [ row [ spacing 8 ] [ setMinutesButton model, setHoursButton model ]
                    , row [ spacing 8 ] [ el [ Font.bold, Font.size 14 ] (text "Group:"), noFilterButton model, filterByDayButton model ]
                    ]
                , newEventPanel 350 sharedState model
                ]


chartPanelForPhone : SharedState -> Model -> Element Msg
chartPanelForPhone sharedState model =
    case sharedState.currentEventList of
        Nothing ->
            Element.none

        Just eventList_ ->
            let
                today =
                    Utility.DateTime.naiveDateStringFromPosix sharedState.currentTime

                events2 =
                    dateFilter today model.dateFilter eventList_

                events =
                    case model.filterState of
                        NoGrouping ->
                            Data.correctTimeZone model.timeZoneOffset events2

                        GroupByDay ->
                            Data.eventsByDay model.timeZoneOffset events2
            in
            column [ Font.size 12, spacing 18, width (px 300) ]
                [ row [ spacing 12 ]
                    [ goToEventsButton model
                    , el [ Font.bold ] (text "Since:")
                    , inputEventDateFilter model
                    ]
                , row [ moveLeft 20 ] [ Graph.barChart (gAPhone model) (prepareData (getScaleFactor model) events) |> Element.html ]
                , row [ spacing 16 ]
                    [ row [ spacing 8 ] [ setMinutesButton model, setHoursButton model ]
                    , row [ spacing 8 ] [ el [ Font.bold, Font.size 14 ] (text "Group:"), noFilterButton model, filterByDayButton model ]
                    ]
                , newEventPanel 300 sharedState model
                ]


newEventPanel : Int -> SharedState -> Model -> Element Msg
newEventPanel w sharedState model =
    column [ Border.width 1, padding 12, spacing 24, width (px w) ]
        [ row [ spacing 12 ] [ submitEventButton, inputValue model ]
        , largeElapsedTimePanel sharedState model
        ]


getScaleFactor : Model -> Float
getScaleFactor model =
    case model.outputUnit of
        Seconds ->
            1

        Minutes ->
            60.0

        Hours ->
            3600.0


gA model =
    let
        yTickMarks_ =
            4
    in
    { graphHeight = 200
    , graphWidth = 400
    , options = [ Color "blue", XTickmarks 7, YTickmarks yTickMarks_, DeltaX 10 ]
    }


gAPhone model =
    let
        yTickMarks_ =
            4
    in
    { graphHeight = 200
    , graphWidth = 300
    , options = [ Color "blue", XTickmarks 4, YTickmarks yTickMarks_, DeltaX 7.5, Scale 1.0 -1.0 ]
    }


floatValueOfEvent : Float -> Event -> Float
floatValueOfEvent scaleFactor_ event =
    event |> .value |> (\x -> x / scaleFactor_)


prepareData : Float -> List Event -> List Float
prepareData scaleFactor_ eventList =
    List.map (floatValueOfEvent scaleFactor_) eventList


type alias Data =
    { index : Float
    , value : Float
    }
