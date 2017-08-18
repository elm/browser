# Elm in the Browser!

This library has two main modules:

  - `Browser` &mdash; for creating Elm programs in the browser
  - `Browser.History` &mdash; tools for single-page apps (SPAs)


## `Browser`

The `Browser` module lets you create Elm programs with the following functions:

  - [`staticPage`](Browser#staticPage)
  - [`sandbox`](Browser#sandbox)
  - [`embed`](Browser#embed)
  - [`fullscreen`](Browser#fullscreen)

These form a smooth learning path, introducing concepts gradually. Start with showing things on screen with `staticPage`, then working with buttons, text input, etc. with `sandbox`, then HTTP and JavaScript interop in `embed`, and finally single-page apps with `fullscreen`. People who use Elm at work also follow a similar path, embedding Elm in JS before writing *everything* in Elm.

To learn more, I highly recommend working through [guide.elm-lang.org][guide] which is built around this learning path!

[guide]: https://guide.elm-lang.org/
