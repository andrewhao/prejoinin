# Prejoinin

A responsive Wejoinin sheet signup widget.

## Installation

    $ elm make
    $ elm package install

## File structure

```
Main.elm         -- Program entrypoint
SignupTable.elm  -- Table widget Elm program
Data/
  Sheet.elm      -- Necessary types for domain models and JSON decoding capabilities
```

## Developing the app

You can compile the app with Elm Live, which provides hot code reloading upon file changes in the dev environment.

    $ elm live Main.elm --open

Else, if you want to use Elm Reactor (dev server) with its awesome time travel debugger, you can run:

    $ elm Reactor
    $ open http://localhost:8000

## Tests

TBD

## Production deploy

TBD
