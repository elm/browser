# Elm in the Browser!

This library has two main modules:

  - `Browser` &mdash; for creating Elm programs in the browser
  - `Browser.History` &mdash; tools for single-page apps (SPAs)


<br>

## Learning Path

**I highly recommend working through [guide.elm-lang.org][guide] to learn how to use Elm.** It is built around a learning path that introduces concepts gradually.

[guide]: https://guide.elm-lang.org/

You can see the outline of that learning path in the `Browser` module. It lets you create Elm programs with the following functions:

  1. [`staticPage`](https://github.com/elm-lang/browser/blob/master/src/Browser.elm#L51) &mdash; learn how to show things with `elm-lang/html`
  2. [`sandbox`](https://github.com/elm-lang/browser/blob/master/src/Browser.elm#L73) &mdash; react to user input, like buttons and checkboxes
  3. [`embed`](https://github.com/elm-lang/browser/blob/master/src/Browser.elm#L108) &mdash; talk to the outside world, like HTTP and JS interop
  4. [`fullscreen`](https://github.com/elm-lang/browser/blob/master/src/Browser.elm#L141) &mdash; create single-page apps

This order works well because important concepts and techniques are introduced at each stage. If you jump ahead, it is like building a house by starting with the roof! So again, **work through [guide.elm-lang.org][guide] to see examples and really *understand* how Elm works!**

This order also works well because it mirrors how most people introduce Elm at work. I usually hear of folks *embedding* Elm in an existing JavaScript project, and if that goes well, perhaps transitioning to doing entire projects in Elm. Having a low-risk transition path like `embed` is crucial if you are serious about using *any* new technology at work!
