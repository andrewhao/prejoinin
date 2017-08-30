# Prejoinin

A responsive Wejoinin sheet signup widget. Uses Elm 0.18.

## Installation

    $ npm run compile

## File structure

```
Main.elm         -- Program entrypoint
SignupTable.elm  -- Table widget Elm program
Data/
  Sheet.elm      -- Necessary types for domain models and JSON decoding capabilities
scss/
  application.scss -- Entrypoint to SCSS styles
js/
  application.js   -- Webpack entrypoint to external JS dependencies.
index.html       -- Wrapper HTML file that loads stylesheets and bootstraps the app.
dist/
  index.html          -- Compiled HTML wrapper
  js/app.js           -- Compiled JS app
  js/bundle.js        -- Additional JS dependencies, compiled via Webpack
  css/application.css -- Compiled CSS
```

## Developing the app

You can compile the app with Elm Live, which provides hot code reloading upon file changes in the dev environment.

    $ elm live Main.elm --output=app.js --open

This is shorthanded to:

    $ npm run elm:live

Else, if you want to use Elm Reactor (dev server) with its awesome time travel debugger, you can run:

    $ elm Reactor
    $ open http://localhost:8000

## JS interface

This app can be embedded in the following way:

```html
<body>
  <div id="wejoinin-root"></div>
  <script src="/dist/js/app.js"></script>
  <script>
      Elm.Main.embed(document.querySelector("#wejoinin-root"), {
        sheetId: "xlogl",
        apiBaseEndpoint: "https://localhost:3000",
        productionMode: true,
        apiKey: "<apiKey>"
      });
  </script>
</body>
```

Note the fields `sheetId`, `apiBaseEndpoint`, `productionMode`, and `apiKey` must be populated.

It is packaged as an [npm package](https://www.npmjs.com/package/prejoinin). Your app should find a DOM node to embed this widget in.

## Ports

### App outputs (JS subscriptions)

`app.ports.modalChanged` sends a boolean value indicating whether a modal is currently open on the page or not, and you will need to throw a `modal-open` class on the `<body>` tag if it is open. Use the implementation in `index.html` as a reference.

`app.ports.sheetUpdated` emits an event every time the table format or data changes.

### App inputs (JS push events)

`app.ports.needsRightScrollerArrow` takes in a `true` or `false` boolean value, indicating that the Elm app needs to display the scroller right arrow. This logic should be bound to JS library logic that detects layout changes when the side scroller column list overflows its layout.

## Tests

TBD

## Staging on Heroku

Deploys to https://wejoinin-ui.herokuapp.com

## Production deploy

TBD
