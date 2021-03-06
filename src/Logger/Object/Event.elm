-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Logger.Object.Event exposing (id, insertedAt, logId, unit, value)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Logger.InputObject
import Logger.Interface
import Logger.Object
import Logger.Scalar
import Logger.ScalarCodecs
import Logger.Union


id : SelectionSet Int Logger.Object.Event
id =
    Object.selectionForField "Int" "id" [] Decode.int


insertedAt : SelectionSet Logger.ScalarCodecs.NaiveDateTime Logger.Object.Event
insertedAt =
    Object.selectionForField "ScalarCodecs.NaiveDateTime" "insertedAt" [] (Logger.ScalarCodecs.codecs |> Logger.Scalar.unwrapCodecs |> .codecNaiveDateTime |> .decoder)


logId : SelectionSet Int Logger.Object.Event
logId =
    Object.selectionForField "Int" "logId" [] Decode.int


unit : SelectionSet String Logger.Object.Event
unit =
    Object.selectionForField "String" "unit" [] Decode.string


value : SelectionSet Float Logger.Object.Event
value =
    Object.selectionForField "Float" "value" [] Decode.float
