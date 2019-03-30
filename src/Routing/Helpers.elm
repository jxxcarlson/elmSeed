module Routing.Helpers exposing (Route(..), parseUrl, reverseRoute, routeParser)

import Url exposing (Url)
import Url.Parser exposing ((</>))


type Route
    = CurrentUserRoute
    | NotFoundRoute


reverseRoute : Route -> String
reverseRoute route =
    case route of
        CurrentUserRoute ->
            "#/"

        _ ->
            "#/"


routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map CurrentUserRoute Url.Parser.top

        --, Url.Parser.map BooksRoute (Url.Parser.s "books")
        ]


parseUrl : Url -> Route
parseUrl url =
    case url.fragment of
        Nothing ->
            CurrentUserRoute

        Just fragment ->
            { url | path = fragment, fragment = Nothing }
                |> Url.Parser.parse routeParser
                |> Maybe.withDefault NotFoundRoute
