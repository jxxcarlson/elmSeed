# Seed

This app is derived Ohanhi's [Shared State Demo](https://github.com/ohanhi/elm-shared-state).  In this example, when the user signs in sign in, his successful sign-in informatiom, e.g. a sesssion token, is stored the parent, shared state, and so is resistant to tamperign.

The app's functionality is deliberately restricted, so that it can, if desired, serve as a templaste for others.

## Theory

Please consult Ohahnhi's README to understand the logic behind the shared state architecture.  The main point of Ohanhi's setup is to use an augmented `update` function with the type signature:

```
update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )

```

This function can be used in a child page, and the `SharedStateUpdate` constructor can be used to update the share state, "the parent," from the child.


## Installation

For now: `elm make src/Main.elm --output=Main.js`.  Then start `elm reactor` and click on `index.html`


## Graphql

**NOTE**  When running `npm run api` to generate Elm code
from the schema, authentication must be disabled.  For
the moment, this is very clunky: switch `call` and `call1`
in `LogServerWeb.Context` of the server.

[GraphQL/Learn](https://graphql.github.io/learn/queries/)

[Example: curl, authorization](https://developer.github.com/v4/guides/intro-to-graphql/)

[Elixir-Absinthe tutorial](https://www.howtographql.com/graphql-elixir/0-introduction/)

[GraphQL Playground (Web)](https://www.graphqlbin.com/v2/new)