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

Note the three fields, `sheetId`, `apiBaseEndpoint` and `productionMode`, and `apiKey` must be populated.

It is packaged as an [npm package](https://www.npmjs.com/package/prejoinin). Your app should find a DOM node to embed this widget in.

## Ports

This app requires a JS listener on the `app.ports.modalChanged` port. This will send a boolean value indicating whether a modal is currently open on the page or not, and you will need to throw a `modal-open` class on the `<body>` tag if it is open. Use the implementation in `index.html` as a reference.

## Tests

TBD

## Staging on Heroku

Deploys to https://wejoinin-ui.herokuapp.com

## Production deploy

TBD
