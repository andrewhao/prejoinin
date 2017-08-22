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
index.html       -- Wrapper HTML file that loads stylesheets and bootstraps the app.
dist/
  index.html     -- Compiled HTML wrapper
  js/app.js      -- Compiled JS app
  css/application.css -- Compiled CSS
```

## Developing the app

You can compile the app with Elm Live, which provides hot code reloading upon file changes in the dev environment.

    $ elm live Main.elm --output=app.js --open

Else, if you want to use Elm Reactor (dev server) with its awesome time travel debugger, you can run:

    $ elm Reactor
    $ open http://localhost:8000

## JS interface

This app can be embedded in the following way:

```html
<body>
  <div id="wejoinin-root"></div>
  <script src="/dist/js/app.js"></script>
  <script>Elm.Main.embed(document.querySelector("#wejoinin-root"));</script>
</body>
```

It is packaged as an [npm package](https://www.npmjs.com/package/prejoinin). Your app should embed `

## Tests

TBD

## Staging on Heroku

Deploys to https://wejoinin-ui.herokuapp.com

## Production deploy

TBD
