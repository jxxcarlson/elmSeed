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
