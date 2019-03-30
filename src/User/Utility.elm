module User.Utility exposing (tagsToString)

import User.Types exposing (User)


tagsToString : Maybe User -> String
tagsToString currentUser_ =
    case currentUser_ of
        Nothing ->
            "No tags"

        Just user ->
            user.tags
                |> String.join ", "
